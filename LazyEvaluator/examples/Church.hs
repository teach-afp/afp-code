main = print do
  let one = \ f -> \ x -> f x;
  let two = \ f -> \ x -> f (f x);
  let plus = \ n -> \ m -> \ f -> \ x -> n f (m f x);
  let times = \ n -> \ m -> \ f -> n (m f);
  let three = plus one two;
  let six   = times two three;
  let seven = plus one six;
  let fourtytwo = times six seven;
  fourtytwo (\ x -> x + 1) 0
