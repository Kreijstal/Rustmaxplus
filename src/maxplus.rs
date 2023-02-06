/*impl ops::Deref for i32{
    type Target = DashNumber<i32>;
    fn deref(self) -> DashNumber<i32> { DashNumber::Number(self) }
}*/
#[derive(Debug,Copy!)]
struct MaxPlusObject<N>(N);
impl<N: Clone> unwrappable<N> for MaxPlusObject<N> {
    fn unwrap(&self) -> N {
        self.0.clone()
    }
}
impl<T> From<T> for MaxPlusObject<T> {
    fn from(item: T) -> Self {
        MaxPlusObject(item)
    }
}
/*impl<N:Copy> unwrappable<N> for MaxPlusObject<N>
{
    fn unwrap(&self) -> N {
        self.0
    }
}*/

impl<N: Clone> ops::Add for MaxPlusObject<N>
where
    N: Ord,
{
    type Output = MaxPlusObject<N>;
    fn add(self, rhs: MaxPlusObject<N>) -> MaxPlusObject<N> {
        // println!("> Foo.add(Bar) was called");
        MaxPlusObject(self.unwrap().max(rhs.unwrap()))
    }
}
impl<N: Clone> ops::Add<N> for MaxPlusObject<N>
where
    N: Ord,
{
    type Output = N;
    fn add(self, rhs: N) -> Self::Output {
        // println!("> Foo.add(Bar) was called");
        self.unwrap().max(rhs)
    }
}
impl<N: Clone> ops::Add<N> for MaxPlusObject<DashNumber<N>>
where
    N: PartialOrd,
{
    type Output = DashNumber<N>;
    fn add(self, rhs: N) -> Self::Output {
        // println!("> Foo.add(Bar) was called");
        if &self.unwrap() > &rhs {
            self.unwrap()
        } else {
            rhs.into()
        }
    }
}
impl<N: Clone> ops::Add<N> for &MaxPlusObject<N>
where
    N: Ord,
{
    type Output = N;
    fn add(self, rhs: N) -> Self::Output {
        // println!("> Foo.add(Bar) was called");
        self.unwrap().max(rhs)
    }
}
impl<N: Clone> ops::Mul for MaxPlusObject<N>
where
    N: ops::Add<Output = N>,
{
    type Output = MaxPlusObject<N>;
    fn mul(self, rhs: MaxPlusObject<N>) -> MaxPlusObject<N> {
        // println!("> Foo.add(Bar) was called");
        MaxPlusObject(self.unwrap() + rhs.unwrap())
    }
}
impl<N: Clone> ops::Mul<MaxPlusObject<N>> for &MaxPlusObject<N>
where
    N: ops::Add<Output = N>,
{
    type Output = MaxPlusObject<N>;
    fn mul(self, rhs: MaxPlusObject<N>) -> MaxPlusObject<N> {
        // println!("> Foo.add(Bar) was called");
        MaxPlusObject(self.unwrap() + rhs.unwrap())
    }
}
impl<N: Clone> ops::Mul for &MaxPlusObject<N>
where
    N: ops::Add<Output = N>,
{
    type Output = MaxPlusObject<N>;
    fn mul(self, rhs: &MaxPlusObject<N>) -> MaxPlusObject<N> {
        // println!("> Foo.add(Bar) was called");
        MaxPlusObject(self.unwrap() + rhs.unwrap())
    }
}
