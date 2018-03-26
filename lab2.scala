

class Krat(kratK: List[Int]) {
	val list = kratK
	def this(k: Int) = this(List(k))

	def in(n: Int): Boolean = inRec(list, n)

	def inRec(l: List[Int], n: Int): Boolean = {
		if (l == Nil) return false
		val x :: xs = l
		if (n%x==0) return true
		return inRec(xs, n)
	}

	def + (q: Krat) = new Krat(list ::: q.list)

	def * (q: Krat) = new Krat(lcm(lcmThisList(this.list) , lcmThisList(q.list)))

	//======= НОК
	def gcd(a: Int, b: Int): Int = {
       if (b == 0) a else gcd(b, a%b)
    }
	def lcm(a: Int, b: Int): Int = {
		(a*b).abs / gcd(a, b)
	}
	def lcmThisList(l: List[Int]): Int = {
		val x1 :: nilTail = l
		if (l.length == 1) return x1
		val x :: a :: xs = l
		if (xs == Nil) return lcm(x, a)

		return lcmThisList(lcm(x, a) :: xs)
	}

}

class KratFactor(x: Int) { 
	def in (krat: Krat) = krat.in(x)
}

implicit def intToKrat(i: Int) = new KratFactor(i)
