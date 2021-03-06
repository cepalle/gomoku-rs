use cursive::{Cursive, Printer, XY};
use cursive::theme::{Color, ColorStyle};
use cursive::vec::Vec2;
use cursive::event::{Event, EventResult, MouseEvent, MouseButton, Callback};
use cursive::direction::Direction;
use std::time::SystemTime;
use std::sync::mpsc;
use std::thread;

const GRID_SIZE: usize = 19;
const LEN_CELL: usize = 3;
const NB_DIR: usize = 8;
const ALL_DIR: [(i16, i16); NB_DIR] = [
    (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0),
    (1, 1),
    (-1, -1),
    (1, -1),
    (-1, 1)
];
const OFFSET_LEFT_GAME: usize = 20;

const NB_MASK: usize = 5;
const LEN_MASK: usize = 6;
const MEMO_MASK_WHITE: [[(i16, i8); LEN_MASK]; NB_MASK] = [
    [(-3, CELL_EMPTY), (-2, CELL_WHITE), (-1, CELL_WHITE), (0, CELL_EMPTY), (1, CELL_EMPTY), (120, CELL_EMPTY)],
    [(-2, CELL_EMPTY), (-1, CELL_WHITE), (0, CELL_EMPTY), (1, CELL_WHITE), (2, CELL_EMPTY), (120, CELL_EMPTY)],
    [(-4, CELL_EMPTY), (-3, CELL_WHITE), (-2, CELL_WHITE), (-1, CELL_EMPTY), (0, CELL_EMPTY), (1, CELL_EMPTY)],
    [(-2, CELL_EMPTY), (-1, CELL_WHITE), (0, CELL_EMPTY), (1, CELL_EMPTY), (2, CELL_WHITE), (3, CELL_EMPTY)],
    [(-1, CELL_EMPTY), (0, CELL_EMPTY), (1, CELL_WHITE), (2, CELL_EMPTY), (3, CELL_WHITE), (4, CELL_EMPTY)],
];
const MEMO_MASK_BLACK: [[(i16, i8); LEN_MASK]; NB_MASK] = [
    [(-3, CELL_EMPTY), (-2, CELL_BLACK), (-1, CELL_BLACK), (0, CELL_EMPTY), (1, CELL_EMPTY), (120, CELL_EMPTY)],
    [(-2, CELL_EMPTY), (-1, CELL_BLACK), (0, CELL_EMPTY), (1, CELL_BLACK), (2, CELL_EMPTY), (120, CELL_EMPTY)],
    [(-4, CELL_EMPTY), (-3, CELL_BLACK), (-2, CELL_BLACK), (-1, CELL_EMPTY), (0, CELL_EMPTY), (1, CELL_EMPTY)],
    [(-2, CELL_EMPTY), (-1, CELL_BLACK), (0, CELL_EMPTY), (1, CELL_EMPTY), (2, CELL_BLACK), (3, CELL_EMPTY)],
    [(-1, CELL_EMPTY), (0, CELL_EMPTY), (1, CELL_BLACK), (2, CELL_EMPTY), (3, CELL_BLACK), (4, CELL_EMPTY)],
];

const DEPTH: i16 = 6;
const DEPTH_MALUS: i32 = 100;
const LEN_MAX_LPOS: usize = 24;
const DEPTH_MALUS_LEN_LPOS_MAX: usize = 3;

const SCORE_CAP: i32 = 200;
const SCORE_ALIGN_1: i32 = 1;
const SCORE_ALIGN_2: i32 = 10;
const SCORE_ALIGN_3: i32 = 100;
const SCORE_ALIGN_4: i32 = 1000;
const SCORE_ALIGN_5: i32 = 1000000;

const INF: i32 = std::i32::MAX / 2;
const SCORE_MAX: i32 = INF / 2;
const SCORE_BREAK: i32 = SCORE_ALIGN_5 / 2;
const SCORE_NB_V: i32 = 1;

const CELL_EMPTY: i8 = 0;
const CELL_WHITE: i8 = 1;
const CELL_BLACK: i8 = 2;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Player {
    White,
    Black,
}

#[derive(Clone, Copy)]
pub enum GameMode {
    Solo(Player),
    Multi,
}

pub struct GameView {
    go_grid: [[i8; GRID_SIZE]; GRID_SIZE],
    game_mode: GameMode,
    player_turn: Player,
    ia_time: u128,
    cursor_suggestion: Option<XY<i16>>,
    nb_cap_white: i16,
    nb_cap_black: i16,
    nb_turn: i16,
    end: Option<Option<Player>>,
}

fn player_to_i8(player: Player) -> i8 {
    match player {
        Player::Black => CELL_BLACK,
        Player::White => CELL_WHITE,
    }
}

fn player_to_str(player: Player) -> &'static str {
    match player {
        Player::Black => "black",
        Player::White => "white",
    }
}

fn next_player(player: Player) -> Player {
    match player {
        Player::Black => Player::White,
        Player::White => Player::Black,
    }
}

fn check_pos(grd: &[[i8; GRID_SIZE]; GRID_SIZE], p: XY<i16>, c: i8) -> bool {
    p.x >= 0 && (p.x as usize) < GRID_SIZE && p.y >= 0 && (p.y as usize) < GRID_SIZE && grd[p.y as usize][p.x as usize] == c
}

fn empty_pos(grd: &[[i8; GRID_SIZE]; GRID_SIZE]) -> [[bool; GRID_SIZE]; GRID_SIZE] {
    let mut todo: [[bool; GRID_SIZE]; GRID_SIZE] = [[false; GRID_SIZE]; GRID_SIZE];

    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            todo[y][x] = grd[y][x] == CELL_EMPTY;
        }
    }

    todo
}

fn del_double_three(grd: &[[i8; GRID_SIZE]; GRID_SIZE], vld: &mut [[bool; GRID_SIZE]; GRID_SIZE], c: i8) {
    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            if !vld[y][x] {
                continue;
            }
            vld[y][x] = check_double_three(grd, c, XY { x: x as i16, y: y as i16 });
        }
    }

    fn check_double_three(grd: &[[i8; GRID_SIZE]; GRID_SIZE], c: i8, xy: XY<i16>) -> bool {
        let XY { x, y } = xy;
        let memo = match c {
            CELL_BLACK => &MEMO_MASK_BLACK,
            _ => &MEMO_MASK_WHITE,
        };

        let mut dir_match: [bool; NB_DIR] = [false; NB_DIR];

        for i in 0..NB_DIR {
            let (dx, dy) = ALL_DIR[i];
            for j in 0..NB_MASK {
                let mut b = true;
                for k in 0..LEN_MASK {
                    let (co, cl) = memo[j][k];
                    if co >= 100 {
                        continue;
                    }
                    b = b && check_pos(grd, XY { x: x + dx * co, y: y + dy * co }, cl);
                }
                dir_match[i] = b || dir_match[i];
            }
        }

        let mut nb_double = 0;
        for i in 0..(NB_DIR / 2) {
            if dir_match[i * 2] || dir_match[i * 2 + 1] {
                nb_double += 1;
            }
        }

        nb_double <= 1
    }
}

fn delcap(grd: &mut [[i8; GRID_SIZE]; GRID_SIZE], p: XY<i16>, player: Player) -> i16 {
    let mut nb_del: i16 = 0;

    for i in 0..NB_DIR {
        let (dx, dy) = ALL_DIR[i];

        let xy1: XY<i16> = XY { x: p.x + dx, y: p.y + dy };
        let xy2: XY<i16> = XY { x: p.x + dx * 2, y: p.y + dy * 2 };
        let xy3: XY<i16> = XY { x: p.x + dx * 3, y: p.y + dy * 3 };

        if !check_pos(grd, xy1, player_to_i8(next_player(player))) {
            continue;
        }
        if !check_pos(grd, xy2, player_to_i8(next_player(player))) {
            continue;
        }
        if !check_pos(grd, xy3, player_to_i8(player)) {
            continue;
        }
        grd[xy1.y as usize][xy1.x as usize] = CELL_EMPTY;
        grd[xy2.y as usize][xy2.x as usize] = CELL_EMPTY;
        nb_del += 2;
    }
    nb_del
}

fn countcap(grd: &[[i8; GRID_SIZE]; GRID_SIZE], p: XY<i16>, player: Player) -> i16 {
    let mut nb_del: i16 = 0;

    for i in 0..NB_DIR {
        let (dx, dy) = ALL_DIR[i];

        let xy1: XY<i16> = XY { x: p.x + dx, y: p.y + dy };
        let xy2: XY<i16> = XY { x: p.x + dx * 2, y: p.y + dy * 2 };
        let xy3: XY<i16> = XY { x: p.x + dx * 3, y: p.y + dy * 3 };

        if !check_pos(grd, xy1, player_to_i8(next_player(player))) {
            continue;
        }
        if !check_pos(grd, xy2, player_to_i8(next_player(player))) {
            continue;
        }
        if !check_pos(grd, xy3, player_to_i8(player)) {
            continue;
        }
        nb_del += 2;
    }
    nb_del
}

fn check_align_5p(grd: &[[i8; GRID_SIZE]; GRID_SIZE], c: i8) -> bool {
    let mut nba: i32;

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: x as i16, y: y as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            if nba >= 5 {
                return true;
            }
        }
    }

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: y as i16, y: x as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            if nba >= 5 {
                return true;
            }
        }
    }

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: (x + y) as i16, y: y as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            if nba >= 5 {
                return true;
            }
        }
    }

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: y as i16, y: (x + y) as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            if nba >= 5 {
                return true;
            }
        }
    }

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: (x as i16) - (y as i16), y: y as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            if nba >= 5 {
                return true;
            }
        }
    }

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: (GRID_SIZE as i16) - 1 - (y as i16), y: (x + y) as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            if nba >= 5 {
                return true;
            }
        }
    }

    false
}

// /!\ Slow
fn check_end_grd(
    grd: &[[i8; GRID_SIZE]; GRID_SIZE],
    nb_cap_white: i16,
    nb_cap_black: i16,
    player: Player,
    last_move: Option<XY<i16>>,
) -> Option<Player> {
    if check_align_5p(grd, player_to_i8(player)) {
        return Some(player);
    }
    if let Some(p) = last_move {
        let mut find = false;
        for i in 0..(NB_DIR / 2) {
            let nba = 1 + check_align_local(grd, p, ALL_DIR[i * 2], player_to_i8(next_player(player)))
                + check_align_local(grd, p, ALL_DIR[i * 2 + 1], player_to_i8(next_player(player)));
            if nba >= 5 {
                find = true;
            }
        }
        if !find {
            return None;
        }
    } else if !check_align_5p(grd, player_to_i8(next_player(player))) {
        return None;
    }

    {
        let mut valid = empty_pos(grd);
        let mut cp_grd: [[i8; GRID_SIZE]; GRID_SIZE];
        del_double_three(grd, &mut valid, player_to_i8(player));

        let nb_cap_player = match player {
            Player::White => nb_cap_white,
            Player::Black => nb_cap_black,
        };

        for y in 0..GRID_SIZE {
            for x in 0..GRID_SIZE {
                if !valid[y][x] {
                    continue;
                }
                cp_grd = *grd;
                let nb_del = delcap(&mut cp_grd, XY { x: x as i16, y: y as i16 }, player);
                if nb_del == 0 {
                    continue;
                }
                if nb_cap_player + nb_del >= 10 {
                    return None;
                }
                if !check_align_5p(&cp_grd, player_to_i8(next_player(player))) {
                    return None;
                }
            }
        }
    }

    Some(next_player(player))
}

// SOLVER

fn del_dist_1(v: &[[bool; GRID_SIZE]; GRID_SIZE]) -> [[bool; GRID_SIZE]; GRID_SIZE] {
    let mut todo: [[bool; GRID_SIZE]; GRID_SIZE] = [[false; GRID_SIZE]; GRID_SIZE];

    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            for j in 0..NB_DIR {
                let (dx, dy) = ALL_DIR[j];
                let xx = (x as i16) + dx;
                let yy = (y as i16) + dy;
                todo[y][x] = todo[y][x] || !v[y][x] ||
                    (xx >= 0 && (xx as usize) < GRID_SIZE && yy >= 0 && (yy as usize) < GRID_SIZE &&
                        !v[yy as usize][xx as usize]
                    );
            }
        }
    }

    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            todo[y][x] = todo[y][x] && v[y][x];
        }
    }

    todo
}

fn valid_to_pos(v: &[[bool; GRID_SIZE]; GRID_SIZE]) -> Vec<XY<i16>> {
    let mut todo: Vec<XY<i16>> = Vec::new();

    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            if v[y][x] {
                todo.push(XY { x: x as i16, y: y as i16 });
            }
        }
    }

    todo
}

fn nba_to_score(nba: i32) -> i32 {
    match nba {
        0 => 0,
        1 => SCORE_ALIGN_1,
        2 => SCORE_ALIGN_2,
        3 => SCORE_ALIGN_3,
        4 => SCORE_ALIGN_4,
        _ => SCORE_ALIGN_5,
    }
}

fn check_align_local(grd: &[[i8; GRID_SIZE]; GRID_SIZE], XY { x, y }: XY<i16>, (dx, dy): (i16, i16), c: i8) -> i32 {
    let mut nba: i16 = 1;
    loop {
        if !check_pos(grd, XY { x: x + dx * nba, y: y + dy * nba }, c) {
            break;
        }
        nba += 1;
    }
    (nba - 1) as i32
}

fn scoring_ordoring(
    grd: &[[i8; GRID_SIZE]; GRID_SIZE],
    p: XY<i16>,
    player: Player,
    nb_cap_white: i16,
    nb_cap_black: i16,
) -> i32 {
    let mut score: i32 = 0;

    for i in 0..(NB_DIR / 2) {
        let ab = 1 + check_align_local(grd, p, ALL_DIR[i * 2], CELL_BLACK)
            + check_align_local(grd, p, ALL_DIR[i * 2 + 1], CELL_BLACK);
        let aw = 1 + check_align_local(grd, p, ALL_DIR[i * 2], CELL_WHITE)
            + check_align_local(grd, p, ALL_DIR[i * 2 + 1], CELL_WHITE);

        score += nba_to_score(ab);
        score += nba_to_score(aw);
        let cap_b = countcap(grd, p, Player::Black) as i32;
        score += (cap_b) * SCORE_CAP;
        let cap_w = countcap(grd, p, Player::White) as i32;
        score += (cap_w) * SCORE_CAP;

        if nb_cap_white + (cap_w as i16) >= 10 || nb_cap_black + (cap_b as i16) >= 10 {
            return SCORE_MAX;
        }
    }

    let mut nb_v = 0;
    for i in 0..NB_DIR {
        let (dx, dy) = ALL_DIR[i];

        if check_pos(grd, XY { x: p.x + dx, y: p.y + dy }, player_to_i8(player)) {
            nb_v += 2;
        }
        if check_pos(grd, XY { x: p.x + dx, y: p.y + dy }, player_to_i8(next_player(player))) {
            nb_v += 1;
        }
    }
    score += nb_v * SCORE_NB_V;

    score
}

fn scoring_align(grd: &[[i8; GRID_SIZE]; GRID_SIZE], player: Player) -> i32 {
    let mut score: i32 = 0;
    let c = player_to_i8(player);

    fn check_can_deploy(grd: &[[i8; GRID_SIZE]; GRID_SIZE], p: XY<i16>, pprev: XY<i16>, c: i8) -> bool {
        let XY { x, y } = pprev;
        let dx = p.x - pprev.x;
        let dy = p.y - pprev.y;

        let mut nba1: i16 = 1;
        loop {
            if !(check_pos(grd, XY { x: x + dx * nba1, y: y + dy * nba1 }, c) ||
                check_pos(grd, XY { x: x + dx * nba1, y: y + dy * nba1 }, CELL_EMPTY)) {
                break;
            }
            nba1 += 1;
        }

        let mut nba2: i16 = 0;
        loop {
            if !(check_pos(grd, XY { x: x - dx * nba2, y: y - dy * nba2 }, c) ||
                check_pos(grd, XY { x: x - dx * nba2, y: y - dy * nba2 }, CELL_EMPTY)) {
                break;
            }
            nba2 += 1;
        }

        nba1 + nba2 - 2 >= 5
    }

    fn score_for(f: fn(usize, usize) -> XY<i16>, score: &mut i32, c: i8, grd: &[[i8; GRID_SIZE]; GRID_SIZE]) {
        let mut nba: i32;
        let mut last_bad_empty: bool;

        for x in 0..(GRID_SIZE + 1) {
            last_bad_empty = false;
            nba = 0;
            for y in 0..(GRID_SIZE + 1) {
                let p = f(x, y);

                if check_pos(grd, p, c) {
                    nba += 1;
                } else if check_pos(grd, p, CELL_EMPTY) {
                    let mut ds = nba_to_score(nba);
                    if nba >= 3 {
                        if last_bad_empty {
                            ds *= 9;
                        }
                        if !check_can_deploy(grd, p, f(x, y - 1), c) {
                            ds /= 22;
                        }
                    }
                    *score += ds;
                    nba = 0;
                    last_bad_empty = true;
                } else {
                    let mut ds = nba_to_score(nba);
                    if nba >= 3 {
                        if !last_bad_empty {
                            ds /= 2;
                        }
                        if !check_can_deploy(grd, p, f(x, y - 1), c) {
                            ds /= 22;
                        }
                    }
                    *score += ds;
                    nba = 0;
                    last_bad_empty = false;
                }
            }
        }
    }

    fn b1(x: usize, y: usize) -> XY<i16> {
        XY { x: x as i16, y: y as i16 }
    }
    fn b2(x: usize, y: usize) -> XY<i16> {
        XY { x: y as i16, y: x as i16 }
    }
    fn b3(x: usize, y: usize) -> XY<i16> {
        XY { x: (x + y) as i16, y: y as i16 }
    }
    fn b4(x: usize, y: usize) -> XY<i16> {
        XY { x: y as i16, y: (x + y) as i16 }
    }
    fn b5(x: usize, y: usize) -> XY<i16> {
        XY { x: (x as i16) - (y as i16), y: y as i16 }
    }
    fn b6(x: usize, y: usize) -> XY<i16> {
        XY { x: (GRID_SIZE as i16) - 1 - (y as i16), y: (x + y) as i16 }
    }

    score_for(b1, &mut score, c, grd);
    score_for(b2, &mut score, c, grd);
    score_for(b3, &mut score, c, grd);
    score_for(b4, &mut score, c, grd);
    score_for(b5, &mut score, c, grd);
    score_for(b6, &mut score, c, grd);

    score
}

fn scoring_end(
    grd: &[[i8; GRID_SIZE]; GRID_SIZE],
    nb_cap_white: i16,
    nb_cap_black: i16,
    player: Player,
) -> i32 {
    let mut score: i32 = match player {
        Player::White => ((nb_cap_white - nb_cap_black) as i32) * SCORE_CAP,
        Player::Black => ((nb_cap_black - nb_cap_white) as i32) * SCORE_CAP,
    };
    score += scoring_align(grd, player);
    score -= scoring_align(grd, next_player(player));

    let next_nb_cap_max: i16 = {
        let mut valid = empty_pos(&grd);
        valid = del_dist_1(&valid);
        del_double_three(&grd, &mut valid, player_to_i8(player));
        let lpos = valid_to_pos(&valid);

        let mut next_nb_cap_max: i16 = 0;
        for p in lpos.iter() {
            let c: i16 = countcap(grd, *p, player);
            next_nb_cap_max = next_nb_cap_max.max(c);
        }
        next_nb_cap_max
    };
    score += (next_nb_cap_max as i32) * (SCORE_CAP / 2);

    score
}

fn nega_max(
    grd: &[[i8; GRID_SIZE]; GRID_SIZE],
    nb_cap_white: i16,
    nb_cap_black: i16,
    depth: i16,
    alpha: i32,
    beta: i32,
    player: Player,
    last_move: Option<XY<i16>>,
) -> (XY<i16>, i32) {
    let mut alpha_mut = alpha;
    let mut to_find: (XY<i16>, i32) = (XY { x: (GRID_SIZE / 2) as i16, y: (GRID_SIZE / 2) as i16 }, -INF);

    let score_end: i32 = SCORE_MAX + (depth as i32) * DEPTH_MALUS;
    if nb_cap_black >= 10 {
        if player == Player::Black {
            return (XY { x: 0, y: 0 }, score_end);
        } else {
            return (XY { x: 0, y: 0 }, -score_end);
        }
    }
    if nb_cap_white >= 10 {
        if player == Player::White {
            return (XY { x: 0, y: 0 }, score_end);
        } else {
            return (XY { x: 0, y: 0 }, -score_end);
        }
    }
    if let Some(p) = check_end_grd(grd, nb_cap_white, nb_cap_black, player, last_move) {
        if p == player {
            return (XY { x: 0, y: 0 }, score_end);
        } else {
            return (XY { x: 0, y: 0 }, -score_end);
        }
    }
    if depth <= 0 {
        return (XY { x: 0, y: 0 }, scoring_end(grd, nb_cap_white, nb_cap_black, player));
    }

    let mut lpos_score: Vec<(XY<i16>, i32)> = {
        let mut valid = empty_pos(&grd);
        valid = del_dist_1(&valid);
        del_double_three(&grd, &mut valid, player_to_i8(player));
        let lpos = valid_to_pos(&valid);

        let mut lpos_score: Vec<(XY<i16>, i32)> = Vec::new();
        for p in lpos.iter() {
            lpos_score.push((*p, scoring_ordoring(grd, *p, player, nb_cap_white, nb_cap_black)))
        }
        lpos_score.sort_by_key(|k| k.1);
        lpos_score.reverse();

        let len_pos_max = LEN_MAX_LPOS - (DEPTH - depth) as usize * DEPTH_MALUS_LEN_LPOS_MAX;
        while lpos_score.len() > len_pos_max {
            lpos_score.pop();
        }

        lpos_score
    };

    fn nega_par(
        grd: [[i8; GRID_SIZE]; GRID_SIZE],
        nb_cap_white: i16,
        nb_cap_black: i16,
        depth: i16,
        alpha: i32,
        beta: i32,
        player: Player,
        spos: Option<(XY<i16>, i32)>,
    ) -> Option<(XY<i16>, i32)> {
        if let Some(posc) = spos {
            let (pos, _) = posc;

            let mut cp = grd;
            cp[pos.y as usize][pos.x as usize] = player_to_i8(player);
            let cap = delcap(&mut cp, pos, player);

            let ss = {
                let (_, s) = nega_max(
                    &cp,
                    if player == Player::White { nb_cap_white + cap } else { nb_cap_white },
                    if player == Player::Black { nb_cap_black + cap } else { nb_cap_black },
                    depth - 1,
                    -beta,
                    -alpha,
                    next_player(player),
                    Some(pos),
                );
                -s
            };

            return Some((pos, ss));
        }

        None
    }

    if depth == DEPTH {
        lpos_score.reverse();
        let cp = *grd;
        let (tx, rx) = mpsc::channel();
        let tx1 = mpsc::Sender::clone(&tx);
        let tx2 = mpsc::Sender::clone(&tx);
        let tx3 = mpsc::Sender::clone(&tx);
        let tx4 = mpsc::Sender::clone(&tx);

        let spos1 = lpos_score.pop();
        let spos2 = lpos_score.pop();
        let spos3 = lpos_score.pop();
        let spos4 = lpos_score.pop();

        thread::spawn(move || {
            tx1.send(nega_par(cp, nb_cap_white, nb_cap_black, depth, alpha_mut, beta, player, spos1)).unwrap();
        });
        thread::spawn(move || {
            tx2.send(nega_par(cp, nb_cap_white, nb_cap_black, depth, alpha_mut, beta, player, spos2)).unwrap();
        });
        thread::spawn(move || {
            tx3.send(nega_par(cp, nb_cap_white, nb_cap_black, depth, alpha_mut, beta, player, spos3)).unwrap();
        });
        thread::spawn(move || {
            tx4.send(nega_par(cp, nb_cap_white, nb_cap_black, depth, alpha_mut, beta, player, spos4)).unwrap();
        });

        loop {
            let spos = lpos_score.pop();

            if let None = spos {
                break;
            }

            let score = rx.recv().unwrap();

            if let Some(posc) = score {
                if posc.1 > to_find.1 {
                    to_find = posc;
                }
            }

            let txcp = mpsc::Sender::clone(&tx);
            thread::spawn(move || {
                txcp.send(nega_par(cp, nb_cap_white, nb_cap_black, depth, alpha_mut, beta, player, spos)).unwrap();
            });

            alpha_mut = alpha_mut.max(to_find.1);
            if alpha_mut >= beta || to_find.1 > SCORE_BREAK {
                break;
            }
        }

        let score1 = rx.recv().unwrap();
        let score2 = rx.recv().unwrap();
        let score3 = rx.recv().unwrap();
        let score4 = rx.recv().unwrap();

        if let Some(posc) = score1 {
            if posc.1 > to_find.1 {
                to_find = posc;
            }
        }
        if let Some(posc) = score2 {
            if posc.1 > to_find.1 {
                to_find = posc;
            }
        }
        if let Some(posc) = score3 {
            if posc.1 > to_find.1 {
                to_find = posc;
            }
        }
        if let Some(posc) = score4 {
            if posc.1 > to_find.1 {
                to_find = posc;
            }
        }
    } else {
        let mut cp: [[i8; GRID_SIZE]; GRID_SIZE];
        for (pos, _) in lpos_score.iter() {
            cp = *grd;
            cp[pos.y as usize][pos.x as usize] = player_to_i8(player);
            let cap = delcap(&mut cp, *pos, player);

            let ss = {
                let (_, s) = nega_max(
                    &cp,
                    if player == Player::White { nb_cap_white + cap } else { nb_cap_white },
                    if player == Player::Black { nb_cap_black + cap } else { nb_cap_black },
                    depth - 1,
                    -beta,
                    -alpha_mut,
                    next_player(player),
                    Some(*pos),
                );
                -s
            };
            if ss > to_find.1 {
                to_find = (*pos, ss);
            }
            alpha_mut = alpha_mut.max(to_find.1);
            if alpha_mut >= beta || to_find.1 > SCORE_BREAK {
                break;
            }
        }
    }

    to_find
}

// SOLVER

impl GameView {
    pub fn new(game_mode: GameMode) -> Self {
        let mut gv = GameView {
            go_grid: [[CELL_EMPTY; GRID_SIZE]; GRID_SIZE],
            game_mode,
            player_turn: Player::Black,
            ia_time: 0,
            cursor_suggestion: None,
            nb_cap_white: 0,
            nb_cap_black: 0,
            nb_turn: 2,
            end: None,
        };

        if let GameMode::Solo(Player::White) = game_mode {
            gv.go_grid[GRID_SIZE / 2][GRID_SIZE / 2] = CELL_BLACK;
            gv.nb_turn += 1;
            gv.player_turn = Player::White;
        }
        gv
    }

    pub fn handle_player_play(&mut self, p: XY<i16>) -> bool {
        self.cursor_suggestion = None;
        if self.end != None {
            return false;
        }

        let mut valid = empty_pos(&self.go_grid);
        del_double_three(&self.go_grid, &mut valid, player_to_i8(self.player_turn));
        if !valid[p.y as usize][p.x as usize] {
            return false;
        }

        self.go_grid[p.y as usize][p.x as usize] = player_to_i8(self.player_turn);
        let cap = delcap(&mut self.go_grid, p, self.player_turn);

        if self.player_turn == Player::Black {
            self.nb_cap_black += cap;
        } else {
            self.nb_cap_white += cap;
        }

        self.player_turn = next_player(self.player_turn);
        self.nb_turn += 1;

        if self.nb_cap_black >= 10 {
            self.end = Some(Some(Player::Black));
            return false;
        }
        if self.nb_cap_white >= 10 {
            self.end = Some(Some(Player::White));
            return false;
        }
        if let Some(p) = check_end_grd(&self.go_grid, self.nb_cap_white, self.nb_cap_black, self.player_turn, None) {
            self.end = Some(Some(p));
            return false;
        }
        true
    }

    pub fn handle_ia_play(&mut self) {
        self.cursor_suggestion = None;
        if self.end != None {
            return;
        }

        let now = SystemTime::now();

        let (xy_ia, _) = nega_max(
            &self.go_grid,
            self.nb_cap_white,
            self.nb_cap_black,
            DEPTH,
            -INF,
            INF,
            self.player_turn,
            None,
        );

        match now.elapsed() {
            Ok(d) => self.ia_time = d.as_millis(),
            Err(_e) => (),
        }

        self.go_grid[xy_ia.y as usize][xy_ia.x as usize] = player_to_i8(self.player_turn);
        let cap = delcap(&mut self.go_grid, xy_ia, self.player_turn);
        if self.player_turn == Player::Black {
            self.nb_cap_black += cap;
        } else {
            self.nb_cap_white += cap;
        }

        self.player_turn = next_player(self.player_turn);
        self.nb_turn += 1;

        if self.nb_cap_black >= 10 {
            self.end = Some(Some(Player::Black));
            return;
        }
        if self.nb_cap_white >= 10 {
            self.end = Some(Some(Player::White));
            return;
        }
        if let Some(p) = check_end_grd(&self.go_grid, self.nb_cap_white, self.nb_cap_black, self.player_turn, None) {
            self.end = Some(Some(p));
            return;
        }
    }

    pub fn handle_suggestion(&mut self) {
        if self.cursor_suggestion != None || self.end != None {
            return;
        }

        let now = SystemTime::now();
        let (xy_ia, _) = nega_max(
            &self.go_grid,
            self.nb_cap_white,
            self.nb_cap_black,
            DEPTH,
            -INF,
            INF,
            self.player_turn,
            None,
        );
        match now.elapsed() {
            Ok(d) => self.ia_time = d.as_millis(),
            Err(_e) => (),
        }

        self.cursor_suggestion = Some(xy_ia);
    }
}

fn cb_ia(c: &mut Cursive) {
    c.refresh();
    c.on_event(Event::Char('p'));
}

impl cursive::view::View for GameView {
    fn draw(&self, printer: &Printer) {
        for y in 0..GRID_SIZE {
            for x in 0..GRID_SIZE {
                let cell = self.go_grid[y][x];

                let text = match cell {
                    CELL_EMPTY => " o ",
                    CELL_WHITE => "( )",
                    _ => "( )",
                };

                let color_back = match cell {
                    CELL_EMPTY => Color::Rgb(200, 200, 200),
                    CELL_WHITE => Color::RgbLowRes(5, 5, 5),
                    _ => Color::RgbLowRes(0, 0, 0),
                };

                let color_font = match cell {
                    CELL_EMPTY => Color::RgbLowRes(0, 0, 0),
                    CELL_WHITE => Color::RgbLowRes(3, 3, 3),
                    _ => Color::RgbLowRes(2, 2, 2),
                };

                printer.with_color(
                    ColorStyle::new(color_font, color_back),
                    |printer| printer.print((x * LEN_CELL + OFFSET_LEFT_GAME, y), text),
                );
            }
        }

        let mut valid = empty_pos(&self.go_grid);
        del_double_three(&self.go_grid, &mut valid, player_to_i8(self.player_turn));
        for y in 0..GRID_SIZE {
            for x in 0..GRID_SIZE {
                valid[y][x] = !valid[y][x] && self.go_grid[y][x] == CELL_EMPTY;
            }
        }
        let lpos = valid_to_pos(&valid);
        for XY { x, y } in lpos.iter() {
            printer.with_color(
                ColorStyle::new(Color::RgbLowRes(5, 0, 0), Color::Rgb(200, 200, 200)),
                |printer| printer.print(((*x as usize) * LEN_CELL + OFFSET_LEFT_GAME, (*y as usize)), "(X)"),
            );
        }

        if let Some(p) = self.cursor_suggestion {
            printer.with_color(
                ColorStyle::new(Color::RgbLowRes(0, 0, 5), Color::Rgb(255, 200, 200)),
                |printer| printer.print(((p.x as usize) * LEN_CELL + OFFSET_LEFT_GAME, (p.y as usize)), "(?)"),
            );
        }

        fn print_tmp(printer: &Printer, p: (usize, usize), text: &str) {
            let color_back = Color::Rgb(200, 200, 200);
            let color_font = Color::RgbLowRes(0, 0, 0);
            printer.with_color(
                ColorStyle::new(color_font, color_back),
                |printer| printer.print(p, text),
            );
        }

        print_tmp(printer, (0, 1), &format!("Turn N°: {}", (self.nb_turn / 2))[..]);
        print_tmp(printer, (0, 2), &format!("Turn: Player {}", player_to_str(self.player_turn))[..]);
        print_tmp(printer, (0, 3), &format!("Nb cap Black: {}", self.nb_cap_black)[..]);
        print_tmp(printer, (0, 4), &format!("Nb cap White: {}", self.nb_cap_white)[..]);
        print_tmp(printer, (0, 6), &format!("Time IA: {} ms", self.ia_time)[..]);

        if let Some(end) = self.end {
            match end {
                None => printer.with_color(
                    ColorStyle::new(Color::Rgb(200, 200, 200), Color::RgbLowRes(0, 0, 5)),
                    |printer| printer.print((0, 8), "Draw"),
                ),
                Some(Player::Black) => printer.with_color(
                    ColorStyle::new(Color::Rgb(255, 255, 255), Color::RgbLowRes(0, 0, 0)),
                    |printer| printer.print((0, 8), "Player black win!"),
                ),
                Some(Player::White) => printer.with_color(
                    ColorStyle::new(Color::Rgb(0, 0, 0), Color::RgbLowRes(5, 5, 5)),
                    |printer| printer.print((0, 8), "Player white win!")),
            }
        }

        print_tmp(printer, (0, 12), "Mouse left:");
        print_tmp(printer, (1, 13), "Pose a piece");
        print_tmp(printer, (0, 14), "Mouse middle:");
        print_tmp(printer, (1, 15), "IA Suggestion");
    }

    fn required_size(&mut self, _: Vec2) -> Vec2 {
        Vec2::new(GRID_SIZE * LEN_CELL + OFFSET_LEFT_GAME, GRID_SIZE)
    }

    fn on_event(&mut self, event: Event) -> EventResult {
        if self.end != None {
            return EventResult::Ignored;
        }

        match event {
            Event::Mouse {
                offset,
                position,
                event: MouseEvent::Release(btn),
            } => {
                match btn {
                    MouseButton::Middle => self.handle_suggestion(),
                    MouseButton::Left => {
                        let pos = position
                            .checked_sub(offset)
                            .map(|pos| pos.map_x(|x| {
                                if x > OFFSET_LEFT_GAME {
                                    (x - OFFSET_LEFT_GAME) / LEN_CELL
                                } else {
                                    1024
                                }
                            }));

                        if let Some(p) = pos {
                            if p.y < GRID_SIZE && p.x < GRID_SIZE {
                                let b = self.handle_player_play(XY { x: p.x as i16, y: p.y as i16 });
                                if let GameMode::Multi = self.game_mode {
                                    return EventResult::Ignored;
                                }
                                if b {
                                    return EventResult::Consumed(Some(Callback::from_fn(cb_ia)));
                                }
                            }
                        }

                        return EventResult::Ignored;
                    }
                    _ => ()
                }
            }
            Event::Char('p') => {
                self.handle_ia_play();
                if let GameMode::Solo(p) = self.game_mode {
                    if p != self.player_turn {
                        return EventResult::Consumed(Some(Callback::from_fn(cb_ia)));
                    }
                } else {
                    return EventResult::Ignored;
                }
            }
            _ => (),
        }

        EventResult::Ignored
    }

    fn take_focus(&mut self, _: Direction) -> bool {
        true
    }
}
