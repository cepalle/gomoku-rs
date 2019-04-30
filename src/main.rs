extern crate cursive;

use cursive::Cursive;
use cursive::views::{Button, Dialog, LinearLayout, TextView};

#[derive(Clone, Copy)]
enum Player {
    White,
    Black,
}

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
struct Coord {
    x: i8,
    y: i8,
}

struct GameView {
    go_grid: [Cell; 19 * 19],
    game_mode: GameMode,
    player_turn: Player,
    ia_time: f32,
    cursor_suggestion: Option<Coord>,
    nb_cap_white: i8,
    nb_cap_black: i8,
    nb_turn: i32,
}

impl GameView {
    pub fn new(game_mode: GameMode) -> Self {
        GameView {
            go_grid: [Cell::EmptyCell; 19 * 19],
            game_mode,
            player_turn: Player::Black,
            ia_time: 0.0,
            cursor_suggestion: None,
            nb_cap_white: 0,
            nb_cap_black: 0,
            nb_turn: 2,
        }
    }
}

impl cursive::view::View for GameView {
    fn draw(&self, printer: &Printer) {
        for (i, cell) in self.overlay.iter().enumerate() {
            let x = (i % self.board.size.x) * 2;
            let y = i / self.board.size.x;

            let text = match *cell {
                Cell::Unknown => "[]",
                Cell::Flag => "()",
                Cell::Visible(n) => {
                    ["  ", " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8"][n]
                }
            };

            let color = match *cell {
                Cell::Unknown => Color::RgbLowRes(3, 3, 3),
                Cell::Flag => Color::RgbLowRes(4, 4, 2),
                Cell::Visible(1) => Color::RgbLowRes(3, 5, 3),
                Cell::Visible(2) => Color::RgbLowRes(5, 5, 3),
                Cell::Visible(3) => Color::RgbLowRes(5, 4, 3),
                Cell::Visible(4) => Color::RgbLowRes(5, 3, 3),
                Cell::Visible(5) => Color::RgbLowRes(5, 2, 2),
                Cell::Visible(6) => Color::RgbLowRes(5, 0, 1),
                Cell::Visible(7) => Color::RgbLowRes(5, 0, 2),
                Cell::Visible(8) => Color::RgbLowRes(5, 0, 3),
                _ => Color::Dark(BaseColor::White),
            };

            printer.with_color(
                ColorStyle::new(Color::Dark(BaseColor::Black), color),
                |printer| printer.print((x, y), text),
            );
        }
    }

    fn take_focus(&mut self, _: Direction) -> bool {
        true
    }

    fn on_event(&mut self, event: Event) -> EventResult {
        match event {
            Event::Mouse {
                offset,
                position,
                event: MouseEvent::Press(_btn),
            } => {
                // Get cell for position
                if let Some(pos) = self.get_cell(position, offset) {
                    self.focused = Some(pos);
                    return EventResult::Consumed(None);
                }
            }
            Event::Mouse {
                offset,
                position,
                event: MouseEvent::Release(btn),
            } => {
                // Get cell for position
                if let Some(pos) = self.get_cell(position, offset) {
                    if self.focused == Some(pos) {
                        // We got a click here!
                        match btn {
                            MouseButton::Left => return self.reveal(pos),
                            MouseButton::Right => {
                                self.flag(pos);
                                return EventResult::Consumed(None);
                            }
                            MouseButton::Middle => {
                                return self.auto_reveal(pos);
                            }
                            _ => (),
                        }
                    }

                    self.focused = None;
                }
            }
            _ => (),
        }

        EventResult::Ignored
    }

    fn required_size(&mut self, _: Vec2) -> Vec2 {
        self.board.size.map_x(|x| 2 * x)
    }
}

fn display_game(siv: &mut Cursive, is_first: Option<bool>) {
    let mut buttons = LinearLayout::vertical();

    for i in 0..19 {
        let mut h = LinearLayout::horizontal();

        for j in 0..19 {
            h = h.child(TextView::new("0 "))
        }
        buttons = buttons.child(h);
    }

    siv.add_layer(Dialog::around(LinearLayout::horizontal()
        .child(buttons)
        .child(Button::new("Quit game", |s| { s.pop_layer(); })))
        .title("Gommoku"));
}

fn display_turn_choice(siv: &mut Cursive) {
    siv.add_layer(
        Dialog::new()
            .title("Player Turn")
            .padding((2, 2, 1, 1))
            .content(
                LinearLayout::vertical()
                    .child(Button::new_raw(" First (black) ", |s| display_game(s, Some(true))))
                    .child(Button::new_raw(" Second (white) ", |s| display_game(s, Some(false))))
                    .child(Button::new_raw("    Back     ", |s| { s.pop_layer(); })),
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
                    .child(Button::new_raw(" Multiplayer ", |s| display_game(s, None)))
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
