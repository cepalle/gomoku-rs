extern crate cursive;

use cursive::{Cursive, Printer, XY};
use cursive::theme::{Color, ColorStyle};
use cursive::views::{Button, Dialog, LinearLayout, Panel};
use cursive::vec::Vec2;
use cursive::event::{Event, EventResult, MouseEvent};
use cursive::direction::Direction;
use std::time::SystemTime;

const GRID_SIZE: usize = 19;
const NB_CELL: usize = GRID_SIZE * GRID_SIZE;
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
const MEMO_MASK_WHITE: [[(i16, Cell); LEN_MASK]; NB_MASK] = [
    [(-3, Cell::Empty), (-2, Cell::White), (-1, Cell::White), (0, Cell::Empty), (1, Cell::Empty), (120, Cell::Empty)],
    [(-2, Cell::Empty), (-1, Cell::White), (0, Cell::Empty), (1, Cell::White), (2, Cell::Empty), (120, Cell::Empty)],
    [(-4, Cell::Empty), (-3, Cell::White), (-2, Cell::White), (-1, Cell::Empty), (0, Cell::Empty), (1, Cell::Empty)],
    [(-2, Cell::Empty), (-1, Cell::White), (0, Cell::Empty), (1, Cell::Empty), (2, Cell::White), (3, Cell::Empty)],
    [(-1, Cell::Empty), (0, Cell::Empty), (1, Cell::White), (2, Cell::Empty), (3, Cell::White), (4, Cell::Empty)],
];
const MEMO_MASK_BLACK: [[(i16, Cell); LEN_MASK]; NB_MASK] = [
    [(-3, Cell::Empty), (-2, Cell::Black), (-1, Cell::Black), (0, Cell::Empty), (1, Cell::Empty), (120, Cell::Empty)],
    [(-2, Cell::Empty), (-1, Cell::Black), (0, Cell::Empty), (1, Cell::Black), (2, Cell::Empty), (120, Cell::Empty)],
    [(-4, Cell::Empty), (-3, Cell::Black), (-2, Cell::Black), (-1, Cell::Empty), (0, Cell::Empty), (1, Cell::Empty)],
    [(-2, Cell::Empty), (-1, Cell::Black), (0, Cell::Empty), (1, Cell::Empty), (2, Cell::Black), (3, Cell::Empty)],
    [(-1, Cell::Empty), (0, Cell::Empty), (1, Cell::Black), (2, Cell::Empty), (3, Cell::Black), (4, Cell::Empty)],
];

#[derive(Clone, Copy, PartialEq, Eq)]
enum Player {
    White,
    Black,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Cell {
    Black,
    White,
    Empty,
}

#[derive(Clone, Copy)]
enum GameMode {
    Solo(Player),
    Multi,
}

struct GameView {
    go_grid: [Cell; NB_CELL],
    game_mode: GameMode,
    player_turn: Player,
    ia_time: u128,
    cursor_suggestion: Option<XY<i16>>,
    nb_cap_white: i16,
    nb_cap_black: i16,
    nb_turn: i16,
    end: Option<Option<Player>>,
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

fn cell_of_player(player: Player) -> Cell {
    match player {
        Player::Black => Cell::Black,
        Player::White => Cell::White,
    }
}

fn check_pos(grd: &[Cell; NB_CELL], p: XY<i16>, c: Cell) -> bool {
    (p.x as usize) < GRID_SIZE && (p.y as usize) < GRID_SIZE && grd[xy_to_index(p)] == c
}

fn xy_to_index(p: XY<i16>) -> usize {
    (p.x as usize) + (p.y as usize) * GRID_SIZE
}

fn valide_pos(grd: &[Cell; NB_CELL]) -> [bool; NB_CELL] {
    let mut todo: [bool; NB_CELL] = [true; NB_CELL];

    for i in 0..NB_CELL {
        todo[i] = grd[i] == Cell::Empty;
    }
    return todo;
}

fn del_double_three(grd: &[Cell; NB_CELL], vld: &mut [bool; NB_CELL], c: Cell) {
    for i in 0..NB_CELL {
        if !vld[i] {
            continue;
        }
        vld[i] = check_double_three(grd, c, i);
    }

    fn check_double_three(grd: &[Cell; NB_CELL], c: Cell, index: usize) -> bool {
        let x = (index % GRID_SIZE) as i16;
        let y = (index / GRID_SIZE) as i16;
        let memo = match c {
            Cell::Black => &MEMO_MASK_BLACK,
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
        for i in 0..4 {
            if dir_match[i * 2] || dir_match[i * 2 + 1] {
                nb_double += 1;
            }
        }

        nb_double <= 1
    }
}

fn delcap(grd: &mut [Cell; NB_CELL], p: XY<i16>, player: Player) -> i16 {
    let mut nb_del: i16 = 0;

    for i in 0..NB_DIR {
        let (dx, dy) = ALL_DIR[i];

        let xy1: XY<i16> = XY { x: p.x + dx, y: p.y + dy };
        let xy2: XY<i16> = XY { x: p.x + dx * 2, y: p.y + dy * 2 };
        let xy3: XY<i16> = XY { x: p.x + dx * 3, y: p.y + dy * 3 };

        if !check_pos(grd, xy1, cell_of_player(next_player(player))) {
            continue;
        }
        if !check_pos(grd, xy2, cell_of_player(next_player(player))) {
            continue;
        }
        if !check_pos(grd, xy3, cell_of_player(player)) {
            continue;
        }
        grd[xy_to_index(xy1)] = Cell::Empty;
        grd[xy_to_index(xy2)] = Cell::Empty;
        nb_del += 2;
    }
    nb_del
}

fn check_align_5p(grd: &[Cell; NB_CELL], c: Cell) -> bool {
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

fn check_end_grd(
    grd: &[Cell; NB_CELL],
    nb_cap_white: i16,
    nb_cap_black: i16,
    player: Player,
) -> Option<Player> {
    if check_align_5p(grd, cell_of_player(next_player(player))) {
        return Some(next_player(player));
    }
    if !check_align_5p(grd, cell_of_player(player)) {
        return None;
    }
    let mut valid_next = valide_pos(grd);
    del_double_three(grd, &mut valid_next, cell_of_player(next_player(player)));

    let mut cp_grp: [Cell; NB_CELL];
    let nb_cap_next_player = match next_player(player) {
        Player::White => nb_cap_white,
        Player::Black => nb_cap_black,
    };

    for x in 0..GRID_SIZE {
        for y in 0..GRID_SIZE {
            if !valid_next[x + y * GRID_SIZE] {
                continue;
            }
            cp_grp = *grd;
            let nb_del = delcap(&mut cp_grp, XY { x: x as i16, y: y as i16 }, next_player(player));
            if nb_del == 0 {
                continue;
            }
            if nb_cap_next_player + nb_del >= 10 {
                return None;
            }
            if !check_align_5p(&cp_grp, cell_of_player(player)) {
                return None;
            }
        }
    }

    Some(player)
}

// SOLVER

const DEPTH: i16 = 6;

fn del_dist_1(v: &[bool; NB_CELL]) -> [bool; NB_CELL] {
    let mut todo: [bool; NB_CELL] = [false; NB_CELL];

    for i in 0..NB_CELL {
        let x = (i % GRID_SIZE) as i16;
        let y = (i / GRID_SIZE) as i16;

        for j in 0..NB_DIR {
            let (dx, dy) = ALL_DIR[j];
            let xx = x + dx;
            let yy = y + dy;
            todo[i] = todo[i] || !v[i] ||
                (xx >= 0 && (xx as usize) < GRID_SIZE && yy >= 0 && (yy as usize) < GRID_SIZE &&
                    !v[(xx as usize) + (yy as usize) * GRID_SIZE]
                );
        }
    }

    for i in 0..NB_CELL {
        todo[i] = todo[i] && v[i];
    }

    todo
}

fn valid_to_pos(v: &[bool; NB_CELL]) -> Vec<(i16, i16)> {
    let mut todo: Vec<(i16, i16)> = Vec::new();

    for i in 0..NB_CELL {
        if v[i] {
            let x = i % GRID_SIZE;
            let y = i / GRID_SIZE;
            todo.push((x as i16, y as i16));
        }
    }
    todo
}

const SCORE_CAP: i32 = 200;
const SCORE_ALIGN_1: i32 = 1;
const SCORE_ALIGN_2: i32 = 10;
const SCORE_ALIGN_3: i32 = 100;
const SCORE_ALIGN_4: i32 = 1000;
const SCORE_ALIGN_5: i32 = 100000;

fn scoring_align(grd: &[Cell; NB_CELL], player: Player) -> i32 {
    let mut score: i32 = 0;
    let c = cell_of_player(player);
    let mut nba: i32;

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

    for x in 0..GRID_SIZE {
        nba = 0;
        for y in 0..GRID_SIZE {
            if check_pos(grd, XY { x: x as i16, y: y as i16 }, c) {
                nba += 1;
            } else {
                nba = 0;
            }
            score += nba_to_score(nba);
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
            score += nba_to_score(nba);
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
            score += nba_to_score(nba);
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
            score += nba_to_score(nba);
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
            score += nba_to_score(nba);
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
            score += nba_to_score(nba);
        }
    }

    score
}

fn scoring_end(
    grd: &[Cell; NB_CELL],
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

    score
}
/*
function negamax(node, depth, α, β, color) is
    if depth = 0 or node is a terminal node then
        return color × the heuristic value of node

    childNodes := generateMoves(node)
    childNodes := orderMoves(childNodes)
    value := −∞
    foreach child in childNodes do
        value := max(value, −negamax(child, depth − 1, −β, −α, −color))
        α := max(α, value)
        if α ≥ β then
            break (* cut-off *)
    return value
*/
fn nega_max(
    grd: &[Cell; NB_CELL],
    nb_cap_white: i16,
    nb_cap_black: i16,
    depth: i16,
    beta: i32,
    alpha: i32,
    player: Player,
) -> (XY<i16>, i32) {
    let mut alpha_mut = alpha;
    let mut to_find: (XY<i16>, i32) = (XY { x: 0, y: 0 }, std::i32::MIN / 2);
    let mut cp: [Cell; NB_CELL];

    if nb_cap_black >= 10 {
        if player == Player::Black {
            return (XY { x: 0, y: 0 }, std::i32::MAX / 2);
        } else {
            return (XY { x: 0, y: 0 }, std::i32::MIN / 2);
        }
    }
    if nb_cap_white >= 10 {
        if player == Player::White {
            return (XY { x: 0, y: 0 }, std::i32::MAX / 2);
        } else {
            return (XY { x: 0, y: 0 }, std::i32::MIN / 2);
        }
    }
    if let Some(p) = check_end_grd(grd, nb_cap_white, nb_cap_black, player) {
        if p == player {
            return (XY { x: 0, y: 0 }, std::i32::MAX / 2);
        } else {
            return (XY { x: 0, y: 0 }, std::i32::MIN / 2);
        }
    }

    if depth == 0 {
        return (XY { x: 0, y: 0 }, scoring_end(grd, nb_cap_white, nb_cap_black, player));
    }

    let mut valid = valide_pos(&grd);
    valid = del_dist_1(&valid);
    del_double_three(&grd, &mut valid, cell_of_player(player));
    let lpos = valid_to_pos(&valid);

    // Ordoring

    for (x, y) in lpos.iter() {
        cp = *grd;
        cp[(*x as usize) + (*y as usize) * GRID_SIZE] = cell_of_player(player);
        let cap = delcap(&mut cp, XY { x: *x, y: *y }, player);

        let (_, s) = nega_max(
            &cp,
            if player == Player::White { nb_cap_white + cap } else { nb_cap_white },
            if player == Player::Black { nb_cap_black + cap } else { nb_cap_black },
            depth - 1,
            -alpha_mut,
            -beta,
            next_player(player),
        );
        let ss = -s;
        if ss > to_find.1 {
            to_find = (XY { x: *x, y: *y }, ss);
        }
        alpha_mut = alpha_mut.max(ss);
        if alpha_mut >= beta {
            break;
        }
    }

    to_find
}

// SOLVER

impl GameView {
    pub fn new(game_mode: GameMode) -> Self {
        let mut gv = GameView {
            go_grid: [Cell::Empty; NB_CELL],
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
            gv.go_grid[NB_CELL / 2] = Cell::Black;
            gv.nb_turn += 1;
            gv.player_turn = Player::White;
        }
        gv
    }

    pub fn handle_mouse(&mut self, p: XY<i16>) {
        if self.end != None {
            return;
        }

        let index = xy_to_index(p);

        let mut valid = valide_pos(&self.go_grid);
        del_double_three(&self.go_grid, &mut valid, cell_of_player(self.player_turn));
        if !valid[index] {
            return;
        }

        self.go_grid[index] = cell_of_player(self.player_turn);
        let cap = delcap(&mut self.go_grid, p, self.player_turn);

        if self.player_turn == Player::Black {
            self.nb_cap_black += cap;
        } else {
            self.nb_cap_white += cap;
        }

        if self.nb_cap_black >= 10 {
            self.end = Some(Some(Player::Black));
            return;
        }
        if self.nb_cap_white >= 10 {
            self.end = Some(Some(Player::White));
            return;
        }
        if let Some(p) = check_end_grd(&self.go_grid, self.nb_cap_white, self.nb_cap_black, self.player_turn) {
            self.end = Some(Some(p));
            return;
        }

        self.player_turn = next_player(self.player_turn);
        self.nb_turn += 1;

        if let GameMode::Multi = self.game_mode {
            return;
        }

        let now = SystemTime::now();

        let (xy_ia, _) = nega_max(
            &self.go_grid,
            self.nb_cap_white,
            self.nb_cap_black,
            DEPTH,
            std::i32::MIN / 2,
            std::i32::MAX / 2,
            self.player_turn,
        );

        match now.elapsed() {
            Ok(d) => self.ia_time = d.as_millis(),
            Err(_e) => (),
        }

        let index_ia = xy_to_index(xy_ia);
        self.go_grid[index_ia] = cell_of_player(self.player_turn);
        let cap = delcap(&mut self.go_grid, xy_ia, self.player_turn);
        if self.player_turn == Player::Black {
            self.nb_cap_black += cap;
        } else {
            self.nb_cap_white += cap;
        }

        if self.nb_cap_black >= 10 {
            self.end = Some(Some(Player::Black));
            return;
        }
        if self.nb_cap_white >= 10 {
            self.end = Some(Some(Player::White));
            return;
        }
        if let Some(p) = check_end_grd(&self.go_grid, self.nb_cap_white, self.nb_cap_black, self.player_turn) {
            self.end = Some(Some(p));
            return;
        }

        self.player_turn = next_player(self.player_turn);
        self.nb_turn += 1;
    }
}

impl cursive::view::View for GameView {
    fn draw(&self, printer: &Printer) {
        for (i, cell) in self.go_grid.iter().enumerate() {
            let xp = (i % GRID_SIZE) * LEN_CELL + OFFSET_LEFT_GAME;
            let yp = i / GRID_SIZE;

            let text = match *cell {
                Cell::Empty => " o ",
                Cell::White => "( )",
                Cell::Black => "( )",
            };

            let color_back = match *cell {
                Cell::Empty => Color::Rgb(200, 200, 200),
                Cell::White => Color::RgbLowRes(5, 5, 5),
                Cell::Black => Color::RgbLowRes(0, 0, 0),
            };

            let color_font = match *cell {
                Cell::Empty => Color::RgbLowRes(0, 0, 0),
                Cell::White => Color::RgbLowRes(3, 3, 3),
                Cell::Black => Color::RgbLowRes(2, 2, 2),
            };

            printer.with_color(
                ColorStyle::new(color_font, color_back),
                |printer| printer.print((xp, yp), text),
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
                None => print_tmp(printer, (0, 8), "Draw"),
                Some(p) => print_tmp(printer, (0, 8), &format!("Player {} win!", player_to_str(p))[..]),
            }
        }
    }

    fn required_size(&mut self, _: Vec2) -> Vec2 {
        Vec2::new(GRID_SIZE * LEN_CELL + OFFSET_LEFT_GAME, GRID_SIZE)
    }

    fn on_event(&mut self, event: Event) -> EventResult {
        match event {
            Event::Mouse {
                offset,
                position,
                event: MouseEvent::Release(_btn),
            } => {
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
                        self.handle_mouse(XY { x: p.x as i16, y: p.y as i16 });
                    }
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

fn display_game(siv: &mut Cursive, game_mode: GameMode) {
    siv.add_layer(
        Dialog::new()
            .title("Gomoku")
            .padding((6, 6, 2, 2))
            .content(
                LinearLayout::horizontal()
                    .child(Panel::new(GameView::new(game_mode))),
            )
            .button("Quit game", |s| {
                s.pop_layer();
            }),
    );
}

fn display_turn_choice(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::new()
            .title("Player Turn")
            .padding((2, 2, 1, 1))
            .content(
                LinearLayout::vertical()
                    .child(Button::new_raw(" First (black) ", |s| display_game(s, GameMode::Solo(Player::Black))))
                    .child(Button::new_raw(" Second (white) ", |s| display_game(s, GameMode::Solo(Player::White))))
                    .child(Button::new_raw("     Back      ", |s| { s.pop_layer(); })),
            ),
    );
}

fn display_home(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::new()
            .title("Gomoku")
            .padding((2, 2, 1, 1))
            .content(
                LinearLayout::vertical()
                    .child(Button::new_raw(" Multiplayer ", |s| display_game(s, GameMode::Multi)))
                    .child(Button::new_raw("    Solo    ", display_turn_choice))
                    .child(Button::new_raw("    Exit     ", |s| s.quit())),
            ),
    );
}

fn main() {
    let mut siv = Cursive::default();
    display_home(&mut siv);
    siv.run();
}
