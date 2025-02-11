open Gtk

Application.Init()

let width, height = 50, 50
let cellSize = 16

let initializeGrid () = Array.init width (fun _ -> Array.create height false)
let grid = ref (initializeGrid ())
let running = ref false

let win = new Window "Game of Life"
win.SetDefaultSize(width * cellSize, height * cellSize)
win.SetPosition WindowPosition.Center
win.Destroyed.Add(fun _ -> Application.Quit())

let drawingArea = new DrawingArea()
win.Add drawingArea

drawingArea.Drawn.Add(fun e ->
    let cr = e.Cr

    cr.SetSourceRGB(0.0, 0.0, 0.0) 
    cr.Paint()

    for x in 0 .. width - 1 do
        for y in 0 .. height - 1 do
            match (!grid).[x].[y] with
            | true -> cr.SetSourceRGB(1.0, 1.0, 1.0)
            | false -> cr.SetSourceRGB(0.0, 0.0, 0.0)

            cr.Rectangle(float (x * cellSize), float (y * cellSize), float cellSize, float cellSize)
            cr.Fill()

    cr.SetSourceRGB(0.2, 0.2, 0.2)
    for x in 0 .. width do
        cr.MoveTo(float (x * cellSize), 0.0)
        cr.LineTo(float (x * cellSize), float (height * cellSize))
    for y in 0 .. height do
        cr.MoveTo(0.0, float (y * cellSize))
        cr.LineTo(float (width * cellSize), float (y * cellSize))
    cr.Stroke()
)

drawingArea.AddEvents(int Gdk.EventMask.ButtonPressMask)
drawingArea.ButtonPressEvent.Add(fun args ->
    let x, y = int (args.Event.X / float cellSize), int (args.Event.Y / float cellSize)
    grid := Array.mapi (fun i row -> 
        if i = x then 
            Array.mapi (fun j cell -> if j = y then not cell else cell) row 
        else row) !grid
    drawingArea.QueueDraw()
)

let countNeighbors (grid: bool[][]) x y =
    let offsets = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]
    offsets |> List.sumBy (fun (dx, dy) ->
        let nx, ny = x + dx, y + dy
        match nx, ny with
        | nx, ny when nx >= 0 && ny >= 0 && nx < width && ny < height && grid.[nx].[ny] -> 1
        | _ -> 0
    )

let updateGrid (grid: bool[][]) =
    [| for x in 0 .. width - 1 ->
        [| for y in 0 .. height - 1 ->
            let neighbors = countNeighbors grid x y
            match grid.[x].[y], neighbors with
            | true, 2 | _, 3 -> true
            | _ -> false |] |]

let timeout () =
    if !running then
        grid := updateGrid !grid
        drawingArea.QueueDraw()
    true

type Cell = Alive | Dead

let parseGrid (input: string) =
    let lines = input.Split('\n')
    Array.init width (fun x ->
        Array.init height (fun y ->
            match lines.[y].[x] with
            | 'O' -> Alive
            | '.' -> Dead
            | _ -> failwith "Invalid character in input"
        )
    )

let serializeGrid (grid: Cell[][]) =
    grid
    |> Array.map (fun row ->
        row
        |> Array.map (function
            | Alive -> 'O'
            | Dead -> '.'
        )
        |> System.String
    )
    |> String.concat "\n"

let loadGridFromFile (path: string) =
    let input = System.IO.File.ReadAllText(path)
    parseGrid input

let saveGridToFile (path: string) (grid: Cell[][]) =
    let output = serializeGrid grid
    System.IO.File.WriteAllText(path, output)

GLib.Timeout.Add(100u, timeout) |> ignore

win.KeyPressEvent.Add(fun args ->
    match args.Event.Key with
    | Gdk.Key.space -> running := not !running
    | Gdk.Key.Escape -> Application.Quit()
    | _ -> ()
)

win.ShowAll()
Application.Run()

