package with_monads

import transformers._
import scala.util.Try

object RPN {
    type S = List[Int]
    def rpn(s: String): OptionT[State[S, *], Int] = 
        s.split("\\s+").filter(_.nonEmpty)
            .foldLeft(OptionT.liftF(Monad[State[S, *]].pure(())))((s, op) => s.flatMap(_ => process(op)))
            .flatMap(_ => OptionT.liftF(Stack.size[Int])
                .flatMap(s => 
                    if (s == 1) OptionT.fromOption[State[S, ?], Unit](Some(())) 
                    else OptionT.fromOption[State[S, ?], Unit](None)))
            .flatMap(_ => OptionT(Stack.peek))

    def process(s: String): OptionT[State[S, *], Unit] = {
        val result = if (s == "+") consume(_ + _)
        else if (s == "-") consume(_ - _)
        else if (s == "*") consume(_ * _)
        else if (s == "/") consume(_ / _) // weeee.... divide by zero.......
        else OptionT.fromOption[State[S, *], Int](Try(s.toInt).toOption)
        result.flatMap(i => OptionT.liftF(Stack.push(i)))
    }

    def consume(f: (Int, Int) => Int): OptionT[State[S, *], Int] = 
        for {
            // we are RPN so backwards
            r <- OptionT(Stack.pop[Int])
            l <- OptionT(Stack.pop[Int])
        } yield { f(l, r) } 
}