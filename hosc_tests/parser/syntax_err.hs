list $a :: Nil | Cons $a (list $a);

app = %xs {
  %ys {
    case xs of {
      Nil : ys;
      %Cons z zs : Cons z (app zs xs);
    }
  }
}