open Reprocessing;

type direction =
  | Left
  | Up
  | Down
  | Right;

type animation =
  | Animating(direction, int)
  | Still;

type tile = {
  value: int,
  animating: bool,
};

type grid = array(array(option(tile)));

type state = {
  grid,
  animation,
  score: int,
};

let tileSize = 100;
let gapSize = 20;
let gridSize = 4;
let scoreHeight = 40;
let displaySize = gridSize * tileSize + (gridSize - 1) * gapSize + scoreHeight;
let tileColours = [|
  Utils.color(~r=255, ~g=229, ~b=153, ~a=255),
  Utils.color(~r=255, ~g=183, ~b=50, ~a=255),
  Utils.color(~r=214, ~g=57, ~b=0, ~a=255),
  Utils.color(~r=255, ~g=0, ~b=0, ~a=255),
  Utils.color(~r=255, ~g=0, ~b=114, ~a=255),
  Utils.color(~r=246, ~g=0, ~b=255, ~a=255),
  Utils.color(~r=187, ~g=0, ~b=255, ~a=255),
  Utils.color(~r=102, ~g=0, ~b=255, ~a=255),
  Utils.color(~r=0, ~g=0, ~b=255, ~a=255),
  Utils.color(~r=0, ~g=97, ~b=255, ~a=255),
  Utils.color(~r=0, ~g=187, ~b=255, ~a=255),
  Utils.color(~r=0, ~g=255, ~b=255, ~a=255),
  Utils.color(~r=0, ~g=255, ~b=148, ~a=255),
  Utils.color(~r=0, ~g=255, ~b=80, ~a=255),
  Utils.color(~r=29, ~g=255, ~b=0, ~a=255),
  Utils.color(~r=161, ~g=255, ~b=0, ~a=255),
  Utils.color(~r=255, ~g=255, ~b=0, ~a=255),
  Utils.color(~r=170, ~g=170, ~b=0, ~a=255),
  Utils.color(~r=100, ~g=100, ~b=0, ~a=255),
|];
let backgroundColour = Utils.color(~r=216, ~g=216, ~b=216, ~a=255);
let emptyTileColour = Utils.color(~r=237, ~g=237, ~b=237, ~a=255);
let speed = 1;

let newTile = ({grid, score} as state) => {
  let maybePositions =
    Array.mapi(
      (y, row) =>
        Array.mapi(
          (x, tile) =>
            switch (tile) {
            | Some(_) => None
            | None => Some((x, y))
            },
          row,
        ),
      grid,
    );

  let empties =
    Array.fold_left(
      (acc, row) =>
        Array.fold_left(
          (acc, tile) =>
            switch (tile) {
            | Some(pos) => [pos, ...acc]
            | None => acc
            },
          acc,
          row,
        ),
      [],
      maybePositions,
    );

  let (newX, newY) = List.nth(empties, Random.int(List.length(empties)));
  let value = Random.int(4) == 0 ? 2 : 1;
  let newScore = score + Utils.pow(~base=2, ~exp=value);

  {
    ...state,
    grid:
      Array.mapi(
        (y, row) =>
          Array.mapi(
            (x, tile) =>
              x == newX && y == newY ? Some({value, animating: false}) : tile,
            row,
          ),
        grid,
      ),
    score: newScore,
  };
};

let setup = env => {
  Env.size(~width=displaySize, ~height=displaySize, env);
  let grid = Array.make_matrix(gridSize, gridSize, None);
  {grid, animation: Still, score: 0} |> newTile |> newTile;
};

let drawTileBox =
    (~block as (xBlock, yBlock), ~offset=?, ~text=?, colour, env) => {
  let (x, y) =
    switch (offset) {
    | Some((x, y)) => (x, y + scoreHeight)
    | None => (0, scoreHeight)
    };

  let pos = (
    xBlock * (tileSize + gapSize) + x + gapSize,
    yBlock * (tileSize + gapSize) + y + gapSize,
  );
  Draw.fill(colour, env);
  Draw.rect(~pos, ~width=tileSize, ~height=tileSize, env);
  switch (text) {
  | Some(text) => Draw.text(~body=text, ~pos, env)
  | None => ()
  };
};

let addOffset = (dir, deltaTime, pos) => {
  let distance = speed * deltaTime;
  switch (dir) {
  | Right => pos + distance
  | Left => pos - distance
  | Up => pos - distance
  | Down => pos + distance
  };
};

let rec combineRowRev = (~acc=[], row) =>
  switch (row) {
  | [] => acc
  | [t] => [t, ...acc]
  | [Some({value: v1}), Some({value: v2}), ...rest] when v1 == v2 =>
    combineRowRev(
      ~acc=[Some({value: v1 + 1, animating: true}), ...acc],
      [None, ...rest],
    )
  | [Some(t1), t2, ...rest] =>
    combineRowRev(~acc=[Some(t1), ...acc], [t2, ...rest])
  | [None, t, ...rest] => combineRowRev(~acc=[t, ...acc], [None, ...rest])
  };

let combineRow = row => row |> combineRowRev |> List.rev;

let rec checkAnimatingRev = (~acc=[], row) =>
  switch (row) {
  | [] => acc
  | [t] => [t, ...acc]
  | [Some({value: v1}) as t1, Some({value: v2}) as t2, ...rest]
      when v1 == v2 =>
    checkAnimatingRev(~acc=[t1, ...acc], [t2, ...rest])
  | [Some({animating: true}) as t1, t2, ...rest] =>
    checkAnimatingRev(~acc=[t1, ...acc], [t2, ...rest])
  | [Some(t1), Some(t2), ...rest] =>
    checkAnimatingRev(
      ~acc=[Some(t1), ...acc],
      [Some({...t2, animating: false}), ...rest],
    )
  | [Some(t1), None, ...rest] =>
    checkAnimatingRev(~acc=[Some(t1), ...acc], [None, ...rest])
  | [None, t, ...rest] =>
    checkAnimatingRev(~acc=[None, ...acc], [t, ...rest])
  };

let checkAnimating = row =>
  switch (row) {
  | [] => []
  | [Some(t), ...rest] =>
    checkAnimatingRev([Some({...t, animating: false}), ...rest]) |> List.rev
  | [None, ...rest] => checkAnimatingRev([None, ...rest]) |> List.rev
  };

let foldRows = (f, dir, rows) =>
  switch (dir) {
  | Left =>
    rows |> Array.map(row => row |> Array.to_list |> f |> Array.of_list)
  | Right =>
    rows
    |> Array.map(row =>
         row |> Array.to_list |> List.rev |> f |> List.rev |> Array.of_list
       )
  | Down =>
    let lists =
      rows
      |> Array.fold_left(
           (cols, row) => cols |> Array.mapi((i, col) => [row[i], ...col]),
           Array.make(gridSize, []),
         )
      |> Array.map(f);
    Array.init(gridSize, x =>
      Array.init(gridSize, y => lists[y]->List.nth(gridSize - x - 1))
    );
  | Up =>
    let lists =
      Array.fold_right(
        (row, cols) => cols |> Array.mapi((i, col) => [row[i], ...col]),
        rows,
        Array.make(gridSize, []),
      )
      |> Array.map(f);
    Array.init(gridSize, x =>
      Array.init(gridSize, y => lists[y]->List.nth(x))
    );
  };

let allStill =
  Array.fold_left(
    (acc, row) =>
      acc ?
        Array.fold_left(
          (acc, tile) =>
            switch (tile) {
            | Some({animating}) => !animating && acc
            | None => acc
            },
          acc,
          row,
        ) :
        acc,
    true,
  );

let calculateGrid =
    (deltaTime, {grid: oldGrid, animation: oldAnimation} as oldState) =>
  switch (oldAnimation) {
  | Animating(dir, offset) =>
    let newAnimation = Animating(dir, offset + deltaTime * speed);

    switch (newAnimation) {
    | Animating(dir, pos) when pos >= gapSize + tileSize =>
      let newGrid =
        foldRows(x => x |> combineRow |> checkAnimating, dir, oldGrid);
      allStill(newGrid) ?
        {...oldState, animation: Still, grid: newGrid} |> newTile :
        {
          ...oldState,
          animation: Animating(dir, pos - tileSize - gapSize),
          grid: newGrid,
        };
    | Animating(_, _)
    | Still => {...oldState, animation: newAnimation, grid: oldGrid}
    };
  | Still => oldState
  };

let draw = (oldState, env) => {
  let deltaTime = int_of_float(Env.deltaTime(env) *. 1000.0);

  Draw.background(backgroundColour, env);

  let state = calculateGrid(deltaTime, oldState);
  let {grid, score, animation} = state;

  let offset =
    switch (animation) {
    | Still => (0, 0)
    | Animating(Up, o) => (0, - o)
    | Animating(Down, o) => (0, o)
    | Animating(Right, o) => (o, 0)
    | Animating(Left, o) => (- o, 0)
    };

  let scoreText = "Score: " ++ string_of_int(score);
  Draw.text(~body=scoreText, ~pos=(10, 10), env);

  Util.itern(
    y =>
      Util.itern(
        x => drawTileBox(~block=(x, y), emptyTileColour, env),
        gridSize,
      ),
    gridSize,
  );

  grid
  |> Array.iteri((yblock, row) =>
       row
       |> Array.iteri((xblock, maybeTile) => {
            let block = (xblock, yblock);

            switch (maybeTile) {
            | Some({value, animating}) =>
              let text = Utils.pow(~base=2, ~exp=value) |> string_of_int;
              drawTileBox(
                ~block,
                ~offset=animating ? offset : (0, 0),
                ~text,
                tileColours[value - 1],
                env,
              );
            | None => ()
            };
          })
     );

  state;
};

let setAnimating = grid =>
  Array.map(
    Array.map(tile =>
      switch (tile) {
      | Some(tile) => Some({...tile, animating: true})
      | None => None
      }
    ),
    grid,
  );

let keyPressed = ({grid, animation} as state, env) =>
  switch (animation) {
  | Animating(_, _) => state
  | Still =>
    switch (Env.keyCode(env)) {
    | Events.D => {
        ...state,
        grid: setAnimating(grid) |> foldRows(checkAnimating, Right),
        animation: Animating(Right, 0),
      }
    | Events.A => {
        ...state,
        grid: setAnimating(grid) |> foldRows(checkAnimating, Left),
        animation: Animating(Left, 0),
      }
    | Events.S => {
        ...state,
        grid: setAnimating(grid) |> foldRows(checkAnimating, Down),
        animation: Animating(Down, 0),
      }
    | Events.W => {
        ...state,
        grid: setAnimating(grid) |> foldRows(checkAnimating, Up),
        animation: Animating(Up, 0),
      }
    | _ => {...state, grid, animation}
    }
  };

let () = run(~setup, ~draw, ~keyPressed, ());
