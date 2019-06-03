package object lesson16 {
  type Id[A] = A

  type Reader[E, A] = ReaderT[Id, E, A]
  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = ReaderT[Id, E ,A](f)
  }

  type Writer[W, A] = WriterT[Id, W, A]
  object Writer {
    def apply[W, A](aw: (A, W)): Writer[W, A] = WriterT[Id, W, A](aw)
  }

  type State[S, A] = StateT[Id, S, A]
  object State {
    def apply[S, A](f: S => (A, S)): State[S, A] = StateT[Id, S, A](f)
  }

  implicit class PipeOp[A](value: A) {
    def |>[R](fa: A => R): R = fa(value)
  }
}