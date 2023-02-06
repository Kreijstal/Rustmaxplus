mod hello;
#[macro_use]
extern crate eager;

eager_macro_rules!{ $eager_1
    macro_rules! op{
        ( plus ) => { + };
        ( minus ) => { - };
    }

    macro_rules! integer{
        ( one ) => { 1 };
        ( two ) => { 2 };
        ( three ) =>{3};
    }

    macro_rules! calculate{
        ( $lhs:tt $op:tt $rhs:tt ) => {
             eager!{integer!{$lhs} op!{$op} integer!{$rhs}}
        };
    }
}

#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
}

fn main(){
    let x = calculate!(one plus two);
    assert_eq!(3, x);

    println!("Hello, world! {}",hello::add_one(calculate!(one plus three)));
    println!("{}",calculate!(one plus three));
    let a= Point{x:3,y:4};
    println!("{:?}",a)
}
