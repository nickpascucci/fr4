//! Circuit board model types and functions.

use graphics::{draw_state, math, types};
use opengl_graphics::GlGraphics;
use piston::input::RenderArgs;

const RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];
const GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
const BLUE: [f32; 4] = [0.0, 0.0, 1.0, 0.5];
const WHITE: [f32; 4] = [1.0, 1.0, 1.0, 1.0];
const TRANSPARENT: [f32; 4] = [0.0, 0.0, 0.0, 0.0];

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Point {
    pub x: u64,
    pub y: u64,
}
// TODO Nicer Display implementations. One idea is to use Unicode glyphs for shapes, and to not
// print all of their coordinates and details unless asked.

// TODO Units of measure support.
#[derive(Debug, Clone, PartialEq)]
pub enum Shape {
    ///  A rectangle defined by the points of two of its corners.
    Rectangle { xy1: Point, xy2: Point },

    // Circle { c: Point, r: u64 },
    // Arc { c: Point, r: u64, a: u64 },

    // TODO Transformed shapes?
    /// Inverts a shape, so that it removes from the image instead of adds to it.
    Inverted(Box<Shape>),
}

impl Shape {
    pub fn render(
        &self,
        color: types::Color,
        draw_state: &draw_state::DrawState,
        transform: math::Matrix2d,
        gl: &mut GlGraphics,
    ) {
        use graphics::*;
        match self {
            Shape::Rectangle { xy1, xy2 } => {
                let rect = rectangle::rectangle_by_corners(
                    xy1.x as f64,
                    xy1.y as f64,
                    xy2.x as f64,
                    xy2.y as f64,
                );
                Rectangle::new(color).draw(rect, draw_state, transform, gl);
            }
            // Note: It is important that the clip be respected in other shapes' render methods,
            // otherwise masks will not appear correctly. Additionally, the mask will only affect
            // shapes drawn _after_ it.
            Shape::Inverted(s) => {
                s.render(RED, &DrawState::new_clip(), transform, gl);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layered {
    pub layer: usize,
    pub shape: Shape,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Component {
    Board(Layered),
    Pad(Layered),
    SolderMask(Layered),
    SilkScreen(Layered),
    Group(Vec<Component>), // Do groups need transformation data to draw?
}

impl Component {
    pub fn render(&self, gl: &mut GlGraphics, args: &RenderArgs) {
        gl.draw(args.viewport(), |c, gl| {
            let mut layers = self.to_layers();
            layers.sort_unstable_by_key(|(n, _shapes)| *n);
            for (n, shapes) in layers {
                for shape in shapes {
                    // TODO Layer colors
                    let color = match n % 4 {
                        0 => GREEN,
                        1 => RED,
                        2 => BLUE,
                        3 => WHITE,
                        _ => unreachable!(),
                    };
                    shape.render(
                        color,
                        // &c.draw_state, // Default
                        &draw_state::DrawState::new_outside(),
                        // &c.draw_state.blend(draw_state::Blend::Multiply),
                        c.transform,
                        gl,
                    )
                }
            }
        });
    }

    /// Decompose the component into shapes grouped by layer number.
    fn to_layers<'a>(&'a self) -> Vec<(usize, Vec<&'a Shape>)> {
        let mut layers = vec![];

        let add_to_layer =
            |mut layers: Vec<(usize, Vec<&'a Shape>)>, layer: &usize, shape: &'a Shape| {
                let mut layer_idx = None;
                for (idx, (l_num, _)) in layers.iter().enumerate() {
                    if l_num == layer {
                        layer_idx = Some(idx);
                        break;
                    }
                }
                match layer_idx {
                    Some(i) => layers[i].1.push(shape),
                    None => layers.push((*layer, vec![shape])),
                }
                layers
            };

        match self {
            Component::Board(Layered { layer, shape }) => add_to_layer(layers, layer, shape),
            Component::Pad(Layered { layer, shape }) => add_to_layer(layers, layer, shape),
            Component::SolderMask(Layered { layer, shape }) => add_to_layer(layers, layer, shape),
            Component::SilkScreen(Layered { layer, shape }) => add_to_layer(layers, layer, shape),
            Component::Group(components) => {
                for cmpt in components {
                    let cmpt_layers = cmpt.to_layers();
                    for (l_num, shapes) in cmpt_layers {
                        for shape in shapes {
                            layers = add_to_layer(layers, &l_num, shape);
                        }
                    }
                }
                layers
            }
        }
    }
}
