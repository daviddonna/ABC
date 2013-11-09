let log_on = ref true

let set_logging = (:=) log_on

module type GENETIC = sig
  type t
  val make         : 'a -> t
  val fitness      : t -> float
  val mutate       : t array -> int -> t
  val make_vectors : int -> t array
  val print        : t -> unit
end

module type ABC = functor (G : GENETIC) -> sig
  type t
  val create : int -> int -> t
  val iterate : t -> int -> G.t * float
end

module G : GENETIC = struct
  type t = float array

  let make _ = Array.init 10 (fun _ -> Random.float 10.0)

  let clamp x = min 10.0 (max (0.0) x)

  let sum_with f = Array.fold_left (fun total x -> total +. f x) 0.0

  let mean_squared_error t =
    let sum = Array.fold_left (+.) 0.0 t in
    let mean = sum /. float (Array.length t) in
    let deviation x = (x -. mean) ** 2.0 in
    let mean_sum = sum_with deviation t in
    mean_sum /. float (Array.length t)

  let minimize objective_function t =
    let objective_value = objective_function t in
    if objective_value >= 0.0
    then 1.0 /. (objective_value +. 1.0)
    else 1.0 +. abs_float objective_value

  let alpha = 2.0

  let fitness = minimize mean_squared_error

  let mutate array i =
    Array.mapi
      (fun index value ->
        let r = Random.int (Array.length array - 1) in
        let i' = if r < i then r else r + 1 in (* rth non-i index *)
        let dx = abs_float (value -. array.(i').(index)) in
        let phi = Random.float (2.0 *. alpha) -. alpha in
        clamp (value +. phi *. dx))
      array.(i)

  let make_vectors n = Array.init n make

  let print = Array.iter (Printf.printf " %5.2f")

end

module Make : ABC = functor (G : GENETIC) -> struct
  type t = {
    solutions    : G.t array;
    fitnesses    : float array;
    attempts     : int array;
    mutable best : G.t * float;
    observers    : int;
  }

  let max_attempts = 10

  let create workers observers =
    let solutions = Array.init workers G.make in
    let fitnesses = Array.init workers (fun i -> G.fitness solutions.(i)) in
    let attempts = Array.create workers max_attempts in
    let best = ref (solutions.(0), fitnesses.(0)) in
    for i = 1 to Array.length solutions - 1 do
      if fitnesses.(i) > snd !best then best := solutions.(i), fitnesses.(i)
    done;
    {
      solutions;
      fitnesses;
      attempts;
      best = !best;
      observers;
    }

  let print t =
    if !log_on then begin
      Array.iteri
        (fun i row ->
          Printf.printf "%3d %3d  | " i t.attempts.(i);
          G.print row;
          Printf.printf "  | %6.3f\n" (t.fitnesses.(i)))
        t.solutions;
      Printf.printf "   BEST: [ ";
      G.print (fst t.best);
      Printf.printf "  ]%7.3f\n" (snd t.best)
    end

  let work t i =
    let vector = t.solutions.(i) in
    let fitness = t.fitnesses.(i) in
    let vector' = G.mutate t.solutions i in
    let fitness' = G.fitness vector' in
    if fitness' > fitness then begin
      t.solutions.(i) <- vector';
      t.fitnesses.(i) <- fitness';
      t.attempts.(i) <- max_attempts;
      if t.fitnesses.(i) > snd t.best then
        t.best <- vector', fitness';
    end else begin
      t.attempts.(i) <- t.attempts.(i) - 1;
    end;
    if !log_on then
      Printf.printf "work %d %s (%2d)\n"
        i (if fitness' > fitness then "!" else " ") t.attempts.(i)

  let worker_phase t =
    Array.iteri
      (fun i solution -> work t i)
      t.solutions

  let observer_phase t =
    let sum = Array.fold_left (+.) 0.0 t.fitnesses in
    let rec select i r' =
      if r' < t.fitnesses.(i)
      then i
      else select (i + 1) (r' -. t.fitnesses.(i))
    in
    for o = 0 to t.observers - 1
    do
      let i = select 0 (Random.float sum) in
      work t i;
    done

  let scout_phase t =
    Array.iteri
      (fun i remaining ->
        if remaining <= 0 then begin
          if !log_on then Printf.printf "reset %d\n" i;
          t.solutions.(i) <- G.make ();
          t.fitnesses.(i) <- G.fitness t.solutions.(i);
          t.attempts.(i) <- max_attempts
        end)
      t.attempts

  let iteration t =
      worker_phase t;
      observer_phase t;
      scout_phase t;
      print t

  let rec iterate t n =
    if n = 0 then
      t.best
    else begin
      iteration t;
      iterate t (n - 1)
    end

end

module Example = Make(G)

let _ = Random.self_init ();;
let test = Example.create 10 10;;

let vec, fit = Example.iterate test 10000;;
G.print vec;
print_float fit;
print_newline ()
