module roguelike

type Color = System.ConsoleColor

///<summary> Function to print show a given point in the console </summary>
/// <param> a Point (System.Char*Color*Color) </param>
/// <returns> unit </returns>
let PrintPoint ((c,fg,bg)) = 
    System.Console.ForegroundColor <- fg
    System.Console.BackgroundColor <- bg
    printf "%c" c

///<summary> Class to create a canvas to show grapics in Console. </summary>
/// <param> Constructor takes number of desired rows and collumns </param>
/// <returns> unit </returns>
type Canvas(rows:int,cols:int) = 
    
    ///Constructor
    let mutable xy = Array2D.create rows cols ((' ', Color.Black, Color.White))
    member val Rows = rows
    member val Cols = cols

    /// <summary> Resets the canvas to be white </summary>
    /// <returns> Nothing. </returns>
    member this.Reset () =
        xy <- Array2D.create this.Rows this.Cols ((' ', Color.Black, Color.White))

    ///<summary> Setter functions to assign values to a given Point in this Canvas </summary>
    /// <param> a tuple of x-coord, y-coord, a char, a foreground color, and a background color. </param>
    /// <returns> unit </returns>
    member this.Set(x:int,y:int,c:char,fg:Color,bg:Color) = 
            xy.SetValue((c,fg,bg),x,y)
    
    ///<summary> Shows this Canvas in the console and resets color afterwards. Uses PrintPoint. </summary>
    /// <param> unit </param>
    /// <returns> unit </returns>
    member this.Show()=
        System.Console.Clear()
        for y in [0..this.Cols-1] do
            printf "\n"
            for x in [0..this.Rows-1] do 
                PrintPoint (Array2D.get xy x y)
        System.Console.ResetColor()

[<AbstractClass>]
type Entity(x:int, y:int) =

        abstract Position : (int*int) with get, set
        default val Position = (x,y) with get, set
        abstract member RenderOn: Canvas -> unit
        default this.RenderOn (canvas:Canvas) = ()
        
        member this.X = fst this.Position
        member this.Y = snd this.Position


type Player(x: int, y: int) =
    inherit Entity (x,y)
    let maxHealth = 20
    let mutable health = 10
    override this.RenderOn(canvas:Canvas) = canvas.Set(this.X,this.Y,'�',Color.White, Color.Black)

    member this.Damage (damage: int) =
        health <- health-damage
    
    member this.Heal (amount: int) =
        health <- min (health + amount) maxHealth
    
    member this.MoveTo (x: int, y: int) =
        this.Position <- (x,y)

    member this.HitPoints () : int =
        health

    member this.IsDead () : bool = health < 0 // Feels kinda weird that we're alive when we have 0 health, but whatever

    override this.ToString() =
        sprintf "Player(%d, %d)" this.X this.Y

[<AbstractClass>]
type Item (x: int, y: int, occupie:bool) =
    inherit Entity (x,y)
    override this.RenderOn(canvas:Canvas) = ()

    abstract isExit: unit -> bool
    default this.isExit () = false

    abstract member Empty: unit -> bool
    default this.Empty() = false

    abstract member ReplaceAfterInteract: unit -> bool
    default this.ReplaceAfterInteract() = false

    member this.MoveTo(x:int,y:int) = this.Position <- (x,y)

    abstract member InteractWith : Player -> unit
    default this.InteractWith (player:Player) = ()
    
    member this.FullyOccupy() : bool = occupie

type Wall (x:int, y:int) =
    inherit Item(x, y, true)
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.DarkGray)

type Water (x:int, y:int) =
    inherit Item(x, y, false)
    override this.InteractWith (player:Player) = player.Heal 2
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,'~',Color.White,Color.Blue)

type Fire(x:int, y:int) =
    inherit Item(x, y, false)
    override this.InteractWith (player:Player) = player.Damage 2
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,'^',Color.Red, Color.Yellow)

type FleshEatingPlant (x:int, y:int) = 
    inherit Item(x, y, true)
    override this.ReplaceAfterInteract() = true
    override this.InteractWith (player:Player) = 
        player.Damage 5
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.DarkGreen)

type Zombie (x:int, y:int) = 
    inherit Item(x, y, true)
    override this.ReplaceAfterInteract() = true
    override this.InteractWith (player:Player) = 
        player.Damage 5
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,'Z',Color.Green, Color.Gray)


type Exit (x:int, y:int) = 
    inherit Item(x, y, false)
    override this.isExit () = true
    override this.InteractWith (player:Player) = ()
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.Black)

type Empty(x:int,y:int) = 
    inherit Item(x,y, false)
    override this.Empty() = true
    override this.InteractWith (player:Player) = ()
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.White)


//Virker ikke som det skal
//Switch posistion in level
let switch (item1:Item) (item2:Item) (arr2D) =
    //move position internally
    item1.MoveTo(item2.X, item2.Y)
    item2.MoveTo(item1.X, item1.Y)
    //move posistion in Arr
    let placeholder = (Array2D.get arr2D item1.X item1.Y)
    Array2D.set arr2D item1.X item1.X (Array2D.get arr2D item2.X item2.Y)
    Array2D.set arr2D item2.X item2.Y (placeholder)

/// Returns a unless a is outside of bounds, then it returns the bounds.
/// ex. clamp 0 10 -1 => 0
/// ex. clamp 0 10  5 => 5
/// ex. clamp 0 10 99 => 10
let clamp (lower: int) (upper: int) (a: int): int =
    a |> max lower |> min upper

type World (width:int, height:int) = 
    let emp = Empty(-1,-1):>Item
    
    // All items in the world
    let mutable items: Item list = List.Empty
    let canv = new Canvas(width,height)

    member this.GetItem (x:int, y:int): Item =
        printfn "GetItem"
        // We search the list for the item
        items |>
            List.tryFind (fun item -> item.X = x && item.Y = y) |>
            Option.defaultValue emp // NOTE: I think we should use None to represent empty

    member this.AddItem (item:Item) =
        items <- item::items
    
    member this.Play() = 
        let player = Player(0,0)

        let mutable newPlayerPos = (player.X,player.Y)
        let mutable newZPos = (0,0)
        let mutable gameOver = false
        let mutable roundCount = 0

        //Array2D.iteri (fun x y item -> if item = emp then canv.Set(x,y,' ',Color.White, Color.White) else item.RenderOn(canv)) this.Level

        let render () = 
            // Render all items and then player.
            canv.Reset ()
            items |> List.iter (fun item -> item.RenderOn(canv))
            printfn "%A" player
            player.RenderOn(canv)
            canv.Show()
        
        // Initial render (so that we see the world before moving)
        render ()
        
        //gameloop
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

            // Make sure player isn't within a solid item
            // Checks if newPos is Fully Occupied
            if not (this.GetItem(fst newPlayerPos, snd newPlayerPos).FullyOccupy()) then
                player.MoveTo(fst newPlayerPos, snd newPlayerPos)
                
                //Checks if Item Should be removed or not
                if this.GetItem(fst newPlayerPos, snd newPlayerPos).ReplaceAfterInteract() then 
                    this.AddItem(Empty(fst newPlayerPos, snd newPlayerPos))
                
            (this.GetItem(fst newPlayerPos, snd newPlayerPos)).InteractWith(player)
    
            //ZOMBIE attempt at moving enemy VIRKER IKKE
            if (roundCount%2 = 0) then
                items |> List.iter (fun (item:Item) -> 
                    if item :? Zombie then
                        let (x,y) = (item.X, item.Y)

                        let mutable newZPos = (item.X, item.Y)
                        if y<player.Y then newZPos <- (fst newZPos, y+1)
                        elif y>player.Y then newZPos <- (fst newZPos, y-1)
                
                        if  x<player.X then newZPos <- (x+1, snd newZPos)
                        elif x>player.X then newZPos <- (x-1, snd newZPos)
                        
                        if this.GetItem(newZPos).Empty() then
                            item.MoveTo newZPos
                    )

            // Render
            render ()

            //Show Health
            System.Console.ForegroundColor<-System.ConsoleColor.Green
            printfn "Health : %i" (player.HitPoints())
            System.Console.ResetColor()

            //Check if game has ended
            if ((this.GetItem(player.X, player.Y)).isExit() && player.HitPoints() >= 10) then 
                gameOver <- true
                System.Console.Clear()
                printfn "You Escaped! Well Done!"
            elif player.IsDead() then 
                gameOver <- true
                System.Console.Clear()
                printf "You Died! Game Over!"
            
            //Mangler ordenlige interactioner p� plante.

        
