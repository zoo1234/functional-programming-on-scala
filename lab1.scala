val uniq: List[Int] => (List[Int], Boolean)  = {
	case Nil => (Nil, true)
	case (x :: xs) if (xs == Nil) => (List(x), true)
	case (x :: a :: xs) => { 
		if (a < x) 
			(a :: xs, false)
		else{
			if (a == x) 
				uniq(a :: xs) 
			else {
				val (result, bool) = uniq(a :: xs)
				(x :: result, bool)
			}
		}
	}
}