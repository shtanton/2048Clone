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
};

let tileSize = 100;
let gapSize = 20;
let gridSize = 4;
let displaySize = gridSize * tileSize + (gridSize - 1) * gapSize;
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

let setup = env => {
  Env.size(~width=displaySize, ~height=displaySize, env);
  let grid = Array.make_matrix(gridSize, gridSize, None);
  grid[0][0] = Some({value: 1, animating: false});
  grid[1][0] = Some({value: 1, animating: false});
  {grid, animation: Still};
};

let drawTileBox =
    (~block as (xBlock, yBlock), ~offset=?, ~text=?, colour, env) => {
  let (x, y) =
    switch (offset) {
    | Some(o) => o
    | None => (0, 0)
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

let rec combineRow = row =>
  switch (row) {
  | [] => []
  | [None, Some(t), ...rest] =>
    List.append([Some({...t, animating: false}), ...rest], [None])
  | [None, ...rest] => List.append(rest, [None])
  | [Some({animating: true} as t), ...rest] =>
    combineRow([Some({...t, animating: false}), ...rest])
  | [
      Some({value: v1, animating: false}),
      Some({value: v2, animating: true}),
      ...rest,
    ]
      when v1 == v2 => [
      Some({value: v1 + 1, animating: false}),
      ...combineRow([None, ...rest]),
    ]
  | [
      Some({animating: false}) as t1,
      Some({value, animating: true}),
      ...rest,
    ] => [
      t1,
      ...combineRow([Some({value, animating: false}), ...rest]),
    ]
  | [Some({animating: false}) as t, ...rest] => [t, ...combineRow(rest)]
  };

let combineRows = (dir, rows) =>
  switch (dir) {
  | Left =>
    rows
    |> Array.map(row => row |> Array.to_list |> combineRow |> Array.of_list)
  | Right =>
    rows
    |> Array.map(row =>
         row
         |> Array.to_list
         |> List.rev
         |> combineRow
         |> List.rev
         |> Array.of_list
       )
  | Down =>
    let lists =
      rows
      |> Array.fold_left(
           (cols, row) => cols |> Array.mapi((i, col) => [row[i], ...col]),
           Array.make(gridSize, []),
         )
      |> Array.map(combineRow);
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
      |> Array.map(combineRow);
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

let newTile = grid => {
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

  Array.mapi(
    (y, row) =>
      Array.mapi(
        (x, tile) =>
          x == newX && y == newY ? Some({value, animating: false}) : tile,
        row,
      ),
    grid,
  );
};

let calculateGrid =
    (deltaTime, {grid: oldGrid, animation: oldAnimation} as oldState) =>
  switch (oldAnimation) {
  | Animating(dir, offset) =>
    let newAnimation = Animating(dir, offset + deltaTime * speed);

    let (animation, grid) =
      switch (newAnimation) {
      | Animating(dir, pos) when pos >= tileSize + gapSize =>
        let newGrid = combineRows(dir, oldGrid);
        allStill(newGrid) ?
          (Still, newTile(newGrid)) :
          (Animating(dir, pos - tileSize - gapSize), newGrid);
      | Animating(_, _) => (newAnimation, oldGrid)
      | Still => (newAnimation, oldGrid)
      };

    {animation, grid};
  | Still => oldState
  };

let draw = (oldState, env) => {
  let deltaTime = int_of_float(Env.deltaTime(env) *. 1000.0);

  Draw.background(backgroundColour, env);

  let {grid, animation} = calculateGrid(deltaTime, oldState);

  let offset =
    switch (animation) {
    | Still => (0, 0)
    | Animating(Up, o) => (0, - o)
    | Animating(Down, o) => (0, o)
    | Animating(Right, o) => (o, 0)
    | Animating(Left, o) => (- o, 0)
    };

  grid
  |> Array.iteri((yblock, row) =>
       row
       |> Array.iteri((xblock, maybeTile) => {
            let block = (xblock, yblock);
            drawTileBox(~block, emptyTileColour, env);

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

  {grid, animation};
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
    | Events.D => {grid: setAnimating(grid), animation: Animating(Right, 0)}
    | Events.A => {grid: setAnimating(grid), animation: Animating(Left, 0)}
    | Events.S => {grid: setAnimating(grid), animation: Animating(Down, 0)}
    | Events.W => {grid: setAnimating(grid), animation: Animating(Up, 0)}
    | _ => {grid, animation}
    }
  };

let () = run(~setup, ~draw, ~keyPressed, ());
