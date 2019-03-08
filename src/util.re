let rec itern = (~acc=0, f, n) => {
  let () = f(acc);
  acc < n - 1 ? itern(~acc=acc + 1, f, n) : ();
};
