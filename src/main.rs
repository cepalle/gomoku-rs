extern crate cursive;

use cursive::Cursive;
use cursive::views::{Button, Dialog, LinearLayout, TextView};

fn main() {
    fn display_board(siv: &mut Cursive, is_first: Option<bool>) {
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
                        .child(Button::new_raw(" First (black) ", |s| display_board(s, Some(true))))
                        .child(Button::new_raw(" Second (white) ", |s| display_board(s, Some(false))))
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
                        .child(Button::new_raw(" Multiplayer ", |s| display_board(s, None)))
                        .child(Button::new_raw("    Solo    ", display_turn_choice))
                        .child(Button::new_raw("    Exit     ", |s| s.quit())),
                ),
        );
    }

    let mut siv = Cursive::default();
    display_home(&mut siv);
    siv.run();
}
