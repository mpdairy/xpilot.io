use shared::xp_map::parse;
use std::fs;

fn main() {
    for name in &["tournament", "blood-music2", "globe", "newdarkhell", "teamball"] {
        let path = format!("maps/{}.xp", name);
        let bytes = fs::read(&path).unwrap();
        let src = String::from_utf8_lossy(&bytes);
        match parse(&src) {
            Ok(m) => println!(
                "{:14}: {}x{} ({}x{} blocks)  walls={}  spawns={}  wrap={}  name={:?}",
                name,
                m.width as i32,
                m.height as i32,
                (m.width / 35.0) as i32,
                (m.height / 35.0) as i32,
                m.walls.len(),
                m.spawns.len(),
                m.edge_wrap,
                m.name
            ),
            Err(e) => println!("{}: ERR {}", name, e),
        }
    }
}
