extern crate cursive;

use cursive::Cursive;
use cursive::views::{Button, Dialog, DummyView,
                     LinearLayout, TextView};

fn main() {
    let mut siv = Cursive::default();

    let mut buttons = LinearLayout::vertical();

    for i in 0..19 {
        let mut h = LinearLayout::horizontal();

        for j in 0..19 {
            h = h.child(TextView::new("# "))
        }
        buttons = buttons.child(h);
    }

    siv.add_layer(Dialog::around(LinearLayout::horizontal()
        .child(buttons)
        .child(DummyView)
        .child(Button::new("Quit", Cursive::quit)))
        .title("Gommoku"));

    siv.run();
}
