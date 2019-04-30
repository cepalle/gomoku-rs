extern crate cursive;

use cursive::{Cursive, Printer, XY};
use cursive::theme::{Color, ColorStyle, BaseColor};
use cursive::views::{Button, Dialog, LinearLayout, TextView, Panel};
use cursive::vec::Vec2;
use cursive::event::{AnyCb, Event, EventResult, MouseEvent};
use cursive::direction::Direction;

const GRID_SIZE: usize = 19;
const NB_CELL: usize = GRID_SIZE * GRID_SIZE;
const LEN_CELL: usize = 3;
const ALL_DIR: [(i8, i8); 8] = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)];

#[derive(Clone, Copy)]
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
    ia_time: f32,
    cursor_suggestion: Option<XY<i8>>,
    nb_cap_white: i8,
    nb_cap_black: i8,
    nb_turn: i32,
}

fn valide_pos(grd: &[Cell; NB_CELL]) -> [bool; NB_CELL] {
    let mut todo: [bool; NB_CELL] = [true; NB_CELL];

    for i in 0..NB_CELL {
        todo[i] = (grd[i] == Cell::Empty);
    }
    return todo;
}

fn delcap(grd: &mut [Cell; NB_CELL], p: XY<usize>, player: Player) -> i8 {2}

impl GameView {
    pub fn new(game_mode: GameMode) -> Self {
        GameView {
            go_grid: [Cell::Empty; NB_CELL],
            game_mode,
            player_turn: Player::Black,
            ia_time: 0.0,
            cursor_suggestion: None,
            nb_cap_white: 0,
            nb_cap_black: 0,
            nb_turn: 2,
        }
    }

    pub fn handle_mouse(&mut self, p: XY<usize>) {
        let index = p.y * GRID_SIZE + p.x;

        let valid = valide_pos(&self.go_grid);
        if !valid[index] {
            return;
        }

        self.go_grid[index] = match self.player_turn {
            Player::Black => Cell::White,
            Player::White => Cell::Black,
        };

        let cap = delcap(&mut self.go_grid, p, self.player_turn);


        self.player_turn = match self.player_turn {
            Player::Black => Player::White,
            Player::White => Player::Black,
        };
    }
}

impl cursive::view::View for GameView {
    fn draw(&self, printer: &Printer) {
        for (i, cell) in self.go_grid.iter().enumerate() {
            let xp = (i % GRID_SIZE) * LEN_CELL;
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
    }

    fn required_size(&mut self, _: Vec2) -> Vec2 {
        Vec2::new(GRID_SIZE * LEN_CELL, GRID_SIZE)
    }

    fn on_event(&mut self, event: Event) -> EventResult {
        match event {
            Event::Mouse {
                offset,
                position,
                event: MouseEvent::Release(btn),
            } => {
                let pos = position
                    .checked_sub(offset)
                    .map(|pos| pos.map_x(|x| x / LEN_CELL));

                if let Some(p) = pos {
                    if p.y >= 0 && p.y < GRID_SIZE && p.x >= 0 && p.x < GRID_SIZE {
                        self.handle_mouse(p);
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
