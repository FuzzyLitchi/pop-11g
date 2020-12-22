module roguelike

type Color = System.ConsoleColor

/// <summary>
/// Limits the value of a to be between to bounds.
/// examples: 
/// clamp 0 10 -1 => 0
/// clamp 0 10  5 => 5
/// clamp 0 10 99 => 10 
/// </summary>
/// <param name="lower"> Lower bound </param>
/// <param name="upper"> Upper bound </param>
/// <param name="a"> A value to clamp </param>
/// <returns> Returns a, unless a is outside of bounds, then it returns the bounds. </returns>
let clamp (lower: int) (upper: int) (a: int): int =
    a |> max lower |> min upper

/// A canvas to render our entities with.
/// <summary> Creates a canvas to show grapics in Console. </summary>
/// <param name="rows"> The desired amount of rows. </param>
/// <param name="cols"> The desired amount of collumns. </param>
/// <returns> unit </returns>
type Canvas(rows:int,cols:int) =
    let mutable xy = Array2D.create rows cols ((' ', Color.Black, Color.White))
    member val Rows = rows
    member val Cols = cols

    /// <summary> Resets the canvas to be white </summary>
    /// <returns> Nothing. </returns>
    member this.Reset () =
        xy <- Array2D.create this.Rows this.Cols ((' ', Color.Black, Color.White))

    /// <summary> Setter functions to assign values to a given Point in this Canvas </summary>
    /// <param name="x"> x-coordinate of the point. </param>
    /// <param name="y"> y-coordinate of the point. </param>
    /// <param name="c"> The character to show. </param>
    /// <param name="fg"> Foregound color </param>
    /// <param name="bg"> Background color. </param>
    /// <returns> unit </returns>
    member this.Set(x:int,y:int,c:char,fg:Color,bg:Color) = 
            xy.SetValue((c,fg,bg),x,y)

    /// <summary> Shows this Canvas in the console and resets color afterwards. </summary>
    /// <returns> unit </returns>
    member this.Show () =
        System.Console.Clear()
        for y in [0..this.Cols-1] do
            printf "\n"
            for x in xy.[0.., y] do 
                x|>(fun (c,fg,bg) -> 
                        System.Console.ForegroundColor <- fg
                        System.Console.BackgroundColor <- bg
                        printf "%c" c)
        System.Console.ResetColor()
    
    


    
/// An entity in our world.
/// Everything that exists within the world is an entity.
[<AbstractClass>]
type Entity(x:int, y:int) =
    /// This entities' position
    member val Position = (x,y) with get, set
    member this.X = fst this.Position
    member this.Y = snd this.Position

    /// <summary> Move this entity </summary>
    /// <param name="x"> The desired x-coordinate. </param>
    /// <param name="y"> The desired y-coordinate. </param>
    /// <returns> Nothing </returns>
    member this.MoveTo(x:int,y:int) = this.Position <- (x,y)

    /// <summary> Renders this enitity </summary>
    /// <param name="canvas"> The canvas to render the entity onto. </param>
    /// <returns> Nothing </returns>
    abstract member RenderOn: Canvas -> unit
    default this.RenderOn (canvas:Canvas) = ()


/// The player.
/// There should be only one player in our world.
type Player(x: int, y: int) =
    inherit Entity (x,y)

    /// Maximum health
    let maxHealth = 20
    /// Current health
    let mutable health = 10

    /// <see cref="Entity"> Documented in entity </see>
    override this.RenderOn(canvas:Canvas) = canvas.Set(this.X,this.Y,'�',Color.White, Color.Black)

    /// <summary> Decrements the player's health. </summary>
    /// <param name="damage"> How much health to remove. </param>
    /// <returns> Nothing </returns>
    member this.Damage (damage: int) =
        health <- health-damage

    /// <summary> Increment the player's health. </summary>
    /// <param name="damage"> How much health to add. </param>
    /// <returns> Nothing </returns>
    member this.Heal (amount: int) =
        health <- min (health + amount) maxHealth

    /// <summary> Get the player's health. </summary>
    /// <returns> The player's health </returns>
    member this.HitPoints () : int =
        health

    /// <summary> Check if the player is dead. </summary>
    /// <returns> Whether the player is dead </returns> 
    member this.IsDead () : bool =
        health < 0 // Feels kinda weird that we're alive when we have 0 health, but whatever

    /// <summary> Debug function to print the player. </summary>
    /// <returns> Debug string representation. </returns> 
    override this.ToString() =
        sprintf "Player(%d, %d)" this.X this.Y

[<AbstractClass>]
type Item (x: int, y: int, occupy: bool) =
    inherit Entity (x,y)

    /// <summary> Check if this item is the exit. </summary>
    /// <returns> Whether or not the item is the exit. </returns>
    abstract isExit: unit -> bool
    default this.isExit () = false

    /// <summary> Check if we need to remove the item. </summary>
    /// <returns> Whether or not to remove the item after an iteraction. </returns>
    abstract member ReplaceAfterInteract: unit -> bool
    default this.ReplaceAfterInteract() = false

    /// <summary> Make the item interact with the player. Called when the player touches the item. </summary>
    /// <returns> Nothing. </returns>
    abstract member InteractWith : Player -> unit
    default this.InteractWith (player:Player) = ()

    /// <summary> Check if the item fully occupies the space it is in. </summary>
    /// <returns> Whether or not it fully occupies it. </returns>
    member this.FullyOccupy() : bool = occupy

    /// <summary> Used to compute Enemy movement, most items are how ever not moving enemies. </summary>
    /// <returns> Unit </returns>
    abstract member MoveZ : Player*Item list * int * int * int -> unit
    default this.MoveZ (player:Player, items:Item list, roundCount:int, width: int, heigth:int) = ()

type Wall (x:int, y:int) =
    inherit Item(x, y, true)
    /// <see cref="Entity"> Documented in entity </see>d
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.DarkGray)

type Water (x:int, y:int) =
    inherit Item(x, y, false)

    /// Water heals 2 hitpoints
    /// <see cref="Item"> Documented in item </see>
    override this.InteractWith (player:Player) = player.Heal 2

    /// <see cref="Entity"> Documented in entity </see>
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,'~',Color.White,Color.Blue)

type Fire(x:int, y:int) =
    inherit Item(x, y, false)
    let mutable hp = 5

    /// Fire damages 1 hitpoints, and is trampled after 5 interactions.
    /// <see cref="Item"> Documented in item </see>
    override this.InteractWith (player:Player) =
        player.Damage 1
        hp <- hp - 1

    /// Fire is trampled after 5 interactions and should be removed.
    /// <see cref="Item"> Documented in item </see>
    default this.ReplaceAfterInteract() =
        // The fire is trampled when it has 0 hp or less, and therefore it should be removed.
        hp <= 0

    /// <see cref="Entity"> Documented in entity </see>
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,'^',Color.Red, Color.Yellow)

type FleshEatingPlant (x:int, y:int) =
    inherit Item(x, y, true)

    /// The plant dies after biting the player :(
    /// <see cref="Item"> Documented in item </see>
    override this.ReplaceAfterInteract() = true

    /// The plant bites the player for 5 damage
    /// <see cref="Item"> Documented in item </see>
    override this.InteractWith (player:Player) =
        player.Damage 5
    
    /// <see cref="Entity"> Documented in entity </see>
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.DarkGreen)


type Exit (x:int, y:int) = 
    inherit Item(x, y, false)

    /// Exit is an exit
    /// <see cref="Item"> Documented in item </see>
    override this.isExit () = true

    /// <see cref="Entity"> Documented in entity </see>
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.Black)

type Zombie (x:int, y:int) = 
    inherit Item(x, y, true)

    /// The zombie bites the player for 5 damage
    /// <see cref="Item"> Documented in item </see>
    override this.InteractWith (player:Player) =
        player.Damage 5
    /// <see cref="Entity"> Documented in entity </see>
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,'Z',Color.Green, Color.Gray)


    ///Moves this Zombie towards player if its within 5 tiles or walks around randomly. Can move 1 tile on both axis if not blocked, every other turn.
      /// <see cref="Item"> Documented in entity </see>
    override this.MoveZ (player:Player, items:Item list, roundCount:int, width:int, height:int) = 
        
        if (roundCount%2 = 0) then // We move every 2 turns
            
            let (x,y) = (this.X, this.Y)
            let mutable newZPos = (this.X, this.Y)

            //Checks if player is within 10 tiles of this Zombie
            if System.Math.Abs (player.X-this.X)<10 && System.Math.Abs(player.Y-this.Y)<10 then
                   
                // Zombie Chase behavoir

                   // Checks if movement is blocked by item in y direction, resets position if blocked.
                   newZPos <- (fst newZPos, y+(player.Y.CompareTo y))
                   if (items |> List.tryFind (fun item -> item.X = fst newZPos && item.Y = snd newZPos && item.FullyOccupy()) |> Option.isNone) then
                        this.MoveTo newZPos
                   else newZPos <- (this.X, this.Y)

                   // Checks if movement is blocked by item in x direction. 
                   newZPos <- (x+(player.X.CompareTo x), snd newZPos)
                   if (items |> List.tryFind (fun item -> item.X = fst newZPos && item.Y = snd newZPos && item.FullyOccupy()) |> Option.isNone) then // NOTE: I think we should use None to represent empty.Empty() then
                       this.MoveTo newZPos
            else
                //Zombie Random Walk, with within bounds.
                let r = new System.Random()

                newZPos <- (fst newZPos, clamp 0 (height-1) (y+r.Next(-1, 2)))
                if items |> List.tryFind (fun item -> item.X = fst newZPos && item.Y = snd newZPos && item.FullyOccupy() ) |> Option.isNone then
                    this.MoveTo newZPos
                else newZPos <- (x, y)

                // Checks if movement is blocked by item in x direction
                newZPos <- (clamp 0 (width-1) (x+(r.Next(-1, 2))), snd newZPos) 
                if items |> List.tryFind (fun item -> item.X = fst newZPos && item.Y = snd newZPos && item.FullyOccupy()) |> Option.isNone then // NOTE: I think we should use None to represent empty.Empty() then
                    this.MoveTo newZPos



/// The world. This object contains all items and the player.
type World (width:int, height:int) = 
    let player = Player(0,0)

    let mutable width = width
    let mutable height = height

    // All items in the world
    let mutable items: Item list = List.Empty
    let mutable canv = new Canvas(width,height)

    /// <summary> Gets an item in some position </summary>
    /// <param name="x"> x-coordinate of the item </param>
    /// <param name="y"> y-coordinate of the item </param>
    /// <returns> The item at that position, or the empty item. </returns>
    member this.GetItem (x:int, y:int): Item Option =
        // We search the list for the item
        items |>
            List.tryFind (fun item -> item.X = x && item.Y = y)

    /// <summary> Adds an item to the world </summary>
    /// <param name="item"> The item to add </param>
    /// <returns> Nothing </returns>
    member this.AddItem (item:Item) =
        // This implementation allows for one item to be added multiple times.
        // I'm not sure what effects that would have, but it is probably a bad idea.
        items <- item::items

    /// <summary> Removes an item to the world </summary>
    /// <param name="item"> The item to remove </param>
    /// <returns> Nothing </returns>
    member this.RemoveItem (removedItem:Item) =
        printfn "Removed %A" removedItem
        // Keep every item that isn't the item we are removing.
        items <- items |> List.filter (fun item -> item <> removedItem)

    /// <summary> Load items from a file. </summary>
    /// <param name="name"> The name of the level. A file with that name has to be present in ./ </param>
    /// <returns> Which drones have collided during the simulation. </returns>
    member this.OpenLevel (name:string) =
        // Reset items
        items <- List.Empty

        let lines = System.IO.File.ReadLines ("levels/" + name)
        // Width is equal to the amount of characters on a line
        width <- lines |> Seq.head |> String.length
        let mutable y = 0

        for line in lines do
            // Level has to have a constant width
            if line.Length <> width then
                failwith (sprintf "Line %d has a different width than the first" y)

            line |> String.iteri (fun x char ->
                match char with
                // Space is empty
                | ' ' -> ()
                | 'P' -> player.MoveTo (x, y)
                | 'X' -> this.AddItem (Wall (x, y))
                | 'W' -> this.AddItem (Water (x, y))
                | 'F' -> this.AddItem (Fire (x, y))
                | 'E' -> this.AddItem (Exit (x, y))
                | 'Z' -> this.AddItem (Zombie (x, y))
                | 'V' -> this.AddItem (FleshEatingPlant (x, y))
                | unmatched -> failwith (sprintf "Character %c is not a valid entity" unmatched)
            )

            y <- y + 1

        // Update canvas and world size
        canv <- Canvas (width,y)
        height <- y

    /// <summary> Starts the game loop and continues until death or victory. </summary>
    /// <returns> Nothing. </returns>
    member this.Play () = 

        let mutable gameOver = false
        let mutable roundCount = 0

        let render () = 
            // Render all items and then player.
            canv.Reset ()
            items |> List.iter (fun item -> item.RenderOn canv)
            player.RenderOn canv
            canv.Show ()
            // Print hit points
            let hearts = max 0 (player.HitPoints ()) // Can't show negative hp
            printfn "\nHP: %s" (String.replicate hearts "♥")

        // Initial render (so that we see the world before moving)
        render ()

        // Gameloop
        while not gameOver do 
            roundCount <- roundCount+1

            // Takes player input
            let key = System.Console.ReadKey(true).Key
            // Compute new player position
            let mutable newPlayerPos =
                match key with
                    | System.ConsoleKey.DownArrow -> (player.X, player.Y+1)
                    | System.ConsoleKey.RightArrow -> (player.X+1, player.Y)
                    | System.ConsoleKey.LeftArrow -> (player.X-1, player.Y)
                    | System.ConsoleKey.UpArrow -> (player.X, player.Y-1)
                    // Player didn't press any of the arrow keys
                    | _ -> (player.X, player.Y)

            // Restrict position to be within the boundaries of the world 
            newPlayerPos <- (newPlayerPos |> fst |> clamp 0 (width-1), newPlayerPos |> snd |> clamp 0 (height-1))

            //Moves Zombies
            items |> List.iter (fun item -> item.MoveZ(player, items, roundCount, width, height))
                    

            // Make sure the new player position isn't within a solid item
            let item = this.GetItem(fst newPlayerPos, snd newPlayerPos) // Item at the new position
            if item.IsNone || not (item.Value.FullyOccupy()) then
                player.MoveTo(fst newPlayerPos, snd newPlayerPos)
            if item.IsSome then 
                item.Value.InteractWith(player)  
                //Checks if Item should be removed or not
                if item.Value.ReplaceAfterInteract() then this.RemoveItem(item.Value)
                

            // Render
            render ()

            // Check if game has ended
            if ((this.GetItem(player.X, player.Y)).IsSome && (this.GetItem(player.X, player.Y)).Value.isExit() && player.HitPoints() >= 5) then 
                gameOver <- true
                System.Console.Clear()
                printfn "You Escaped! Well Done!"
            elif player.IsDead() then 
                gameOver <- true
                System.Console.Clear()
                printf "You Died! Game Over!"
