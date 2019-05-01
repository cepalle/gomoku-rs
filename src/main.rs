extern crate cursive;

use cursive::{Cursive, Printer, XY};
use cursive::theme::{Color, ColorStyle};
use cursive::views::{Button, Dialog, LinearLayout, Panel};
use cursive::vec::Vec2;
use cursive::event::{Event, EventResult, MouseEvent};
use cursive::direction::Direction;
use std::time::SystemTime;
use std::thread;
use core::time;

const GRID_SIZE: usize = 19;
const NB_CELL: usize = GRID_SIZE * GRID_SIZE;
const LEN_CELL: usize = 3;
const NB_DIR: usize = 8;
const ALL_DIR: [(i8, i8); NB_DIR] = [
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
const MEMO_MASK_WHITE: [[(i8, Cell); LEN_MASK]; NB_MASK] = [
    [(-3, Cell::Empty), (-2, Cell::White), (-1, Cell::White), (0, Cell::Empty), (1, Cell::Empty), (120, Cell::Empty)],
    [(-2, Cell::Empty), (-1, Cell::White), (0, Cell::Empty), (1, Cell::White), (2, Cell::Empty), (120, Cell::Empty)],
    [(-4, Cell::Empty), (-3, Cell::White), (-2, Cell::White), (-1, Cell::Empty), (0, Cell::Empty), (1, Cell::Empty)],
    [(-2, Cell::Empty), (-1, Cell::White), (0, Cell::Empty), (1, Cell::Empty), (2, Cell::White), (3, Cell::Empty)],
    [(-1, Cell::Empty), (0, Cell::Empty), (1, Cell::White), (2, Cell::Empty), (3, Cell::White), (4, Cell::Empty)],
];
const MEMO_MASK_BLACK: [[(i8, Cell); LEN_MASK]; NB_MASK] = [
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
    cursor_suggestion: Option<XY<i8>>,
    nb_cap_white: u8,
    nb_cap_black: u8,
    nb_turn: u8,
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

fn check_pos(grd: &[Cell; NB_CELL], p: XY<i8>, c: Cell) -> bool {
    p.x >= 0 && (p.x as usize) < GRID_SIZE && p.y >= 0 && (p.y as usize) < GRID_SIZE && grd[xy_to_index(p)] == c
}

fn xy_to_index(p: XY<i8>) -> usize {
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
        let x = (index % GRID_SIZE) as i8;
        let y = (index / GRID_SIZE) as i8;
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

fn delcap(grd: &mut [Cell; NB_CELL], p: XY<i8>, player: Player) -> u8 {
    let mut nb_del: u8 = 0;

    for i in 0..NB_DIR {
        let (dx, dy) = ALL_DIR[i];

        let xy1: XY<i8> = XY { x: p.x + dx, y: p.y + dy };
        let xy2: XY<i8> = XY { x: p.x + dx * 2, y: p.y + dy * 2 };
        let xy3: XY<i8> = XY { x: p.x + dx * 3, y: p.y + dy * 3 };

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

fn check_end_grd(grd: &[Cell; NB_CELL]) -> Option<Player> {
    None
}

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
        }
        gv
    }

    pub fn handle_mouse(&mut self, p: XY<i8>) {
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
        if let Some(p) = check_end_grd(&self.go_grid) {
            self.end = Some(Some(p));
            return;
        }

        if let GameMode::Multi = self.game_mode {
            return;
        }

        let now = SystemTime::now();

        // IA
        thread::sleep(time::Duration::from_millis(500));

        match now.elapsed() {
            Ok(d) => self.ia_time = d.as_millis(),
            Err(_e) => (),
        }

        if self.nb_cap_black >= 10 {
            self.end = Some(Some(Player::Black));
            return;
        }
        if self.nb_cap_white >= 10 {
            self.end = Some(Some(Player::White));
            return;
        }
        if let Some(p) = check_end_grd(&self.go_grid) {
            self.end = Some(Some(p));
            return;
        }
    }
}

impl cursive::view::View for GameView {
    fn draw(&self, printer: &Printer) {
        for (i, cell) in self.go_grid.iter().enumerate() {
            let xp = (i % GRID_SIZE) * LEN_CELL + OFFSET_LEFT_GAME;
            let yp = i / GRID_SIZE;

            let text = match *cell {
                Cell::Empty => " o ",
                Cell::White => "   ",
                Cell::Black => "   ",
            };

            let color_back = match *cell {
                Cell::Empty => Color::Rgb(200, 200, 200),
                Cell::White => Color::RgbLowRes(5, 5, 5),
                Cell::Black => Color::RgbLowRes(0, 0, 0),
            };

            let color_font = match *cell {
                Cell::Empty => Color::RgbLowRes(0, 0, 0),
                Cell::White => Color::RgbLowRes(0, 0, 0),
                Cell::Black => Color::RgbLowRes(5, 5, 5),
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
                        self.handle_mouse(XY { x: p.x as i8, y: p.y as i8 });
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
