extern crate mecab;

use mecab::{node::NodeStat, Node, Tagger};

fn main() {
    dbg!(std::mem::size_of::<Node>());

    let input = "太郎は次郎が持っている本を花子に渡した。";
    println!("INPUT: {}", input);

    let mut tagger =
        Tagger::new("-d /home/cr/Downloads/unidic-csj-3.0.1.1").expect("failed to create tagger");

    // gets tagged result as str
    let result = tagger.parse_to_str(input).expect("failed to parse input");
    println!("RESULT: {}", result);

    // gets N best results as String
    let result = tagger.parse_nbest_to_str(3, input).expect("failed to parse input");

    println!("NBEST:\n{}", result);

    // gets N best in sequence
    let mut nbest = tagger.parse_nbest_init(input).expect("failed to parse input");

    for i in 0..3 {
        if let Some(res) = nbest.next_str() {
            println!("{}:\n{}", i, res);
        }
    }

    for node in tagger.parse_to_node(input).expect("failed to parse input").iter_next() {
        match node.stat() {
            NodeStat::BOS => {
                print!("{} BOS ", node.id());
            }

            NodeStat::EOS => {
                print!("{} EOS ", node.id());
            }

            _ => {
                print!("{} {} ", node.id(), node.surface());
            }
        }

        println!(
            "{} {} {} {} {} {} {} {} {} {} {}",
            node.feature(),
            node.rcattr(),
            node.lcattr(),
            node.pos_id(),
            node.char_type(),
            node.stat(),
            node.is_best(),
            node.alpha(),
            node.beta(),
            node.prob(),
            node.cost()
        );
    }

    // dictionary info
    for dict in tagger.dictionary_info().iter() {
        println!();
        println!("filename: {}", dict.filename());
        println!("charset: {}", dict.charset());
        println!("size: {}", dict.size());
        println!("type: {}", dict.dict_type());
        println!("lsize: {}", dict.left_attrs_size());
        println!("rsize: {}", dict.right_attrs_size());
        println!("version: {}", dict.version());
    }
}
