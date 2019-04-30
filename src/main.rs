extern crate cursive;

use cursive::Cursive;
use cursive::views::{Button, Dialog, LinearLayout, TextView};

enum Player {
    PlayerWhite,
    PlayerBlack,
}

enum Cell {
    PieceBlack,
    PieceWhite,
    EmptyCell,
}

enum GameMode {
    GameSolo(Player),
    GameMulti,
}

struct Coord {
    x: i8,
    y: i8,
}

struct GameState {
    go_grid: [Cell; 19 * 19],
    game_mode: GameMode,
    player_turn: Player,
    ia_time: f32,
    cursor_suggestion: Coord,
    cursor: Coord,
    nb_cap_white: i8,
    nb_cap_black: i8,
    nb_turn: i32,
    end: Option<Option<Player>>,
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
