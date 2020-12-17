module roguelike

type Color = System.ConsoleColor
type Point (c:System.Char,fg:Color,bg:Color) = 
    member val Point = (c,fg,bg) with get, set


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
    let xy = Array2D.create rows cols ((' ', Color.Black, Color.White))
    member val Rows = rows
    member val Cols = cols

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

///// A Position in 2d space
//type Position(x: int, y: int) =
//    /// Read and writeable x/y components
//    member val X = x with get, set
//    member val Y = y with get, set

//    /// <summary> Creates a new position with identical components. </summary>
//    /// <returns> The copy. </returns>
//    member this.Clone (): Position =
//        Position (this.X, this.Y)

//    /// <summary> Debug ToString function. </summary>
//    /// <returns> A human readable string representation of the vector. </returns>
//    override this.ToString () : string =
//        sprintf "Position(%A, %A)" (this.X) (this.Y)

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
    override this.RenderOn(canvas:Canvas) = canvas.Set(this.X,this.Y,'Å',Color.White, Color.Black)

    member this.Damage (damage: int) =
        health <- health-damage
    
    member this.Heal (amount: int) =
        health <- min (health + amount) maxHealth
    
    member this.MoveTo (x: int, y: int) =
        this.Position <- (x,y)

    member this.HitPoints () : int =
        health

    member this.IsDead () : bool = health < 0 // Feels kinda weird that we're alive when we have 0 health, but whatever

[<AbstractClass>]
type Item (x: int, y: int, occupie:bool) =
    inherit Entity (x,y)
    override this.RenderOn(canvas:Canvas) = ()

    abstract member Empty: unit -> bool
    default this.Empty() = false

    abstract member ReplaceAfterInteract: unit -> bool
    default this.ReplaceAfterInteract() = false

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

type Exit (x:int, y:int) = 
    inherit Item(x, y, false)
    override this.InteractWith (player:Player) = ()
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.Black)

type Empty() = 
    inherit Item(1,1, false)
    override this.Empty() = true
    override this.InteractWith (player:Player) = ()
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.White)

type World (length:int,height:int) = 
    let emp = Empty():>Item
    
    let level = Array2D.create (length) (height) (emp)
    let canv = new Canvas(length,height)
    member val Height = height with get
    member val Level = level
    member this.GetItem(x:int,y:int): Item = Array2D.get level x y
    member this.AddItem (item:Item) = this.Level.SetValue (item, item.X, item.Y)
    
    member this.Play() = 
        
        Array2D.iter (fun x -> if x = emp then () else x.RenderOn(canv)) this.Level
        let player = Player(0,0)
        player.RenderOn(canv)
        canv.Show()
        let mutable newPlayerPos = (player.X,player.Y)
        let mutable gameOver = false
        
        //gameloop
        while not gameOver do 
            let key = System.Console.ReadKey().Key
            
            if key = System.ConsoleKey.DownArrow then newPlayerPos <- (player.X, player.Y+1)
            elif key = System.ConsoleKey.RightArrow then newPlayerPos <- (player.X+1, player.Y)
            elif key = System.ConsoleKey.LeftArrow then newPlayerPos <- (player.X-1, player.Y)
            elif key = System.ConsoleKey.UpArrow then newPlayerPos <- (player.X, player.Y-1)

            if fst newPlayerPos >= 0 && fst newPlayerPos < length &&
                snd newPlayerPos >= 0 && fst newPlayerPos < height &&
                not (this.GetItem(fst newPlayerPos, snd newPlayerPos).FullyOccupy()) then 
                    canv.Set(player.X, player.Y,' ', Color.Black, Color.White)
                    player.MoveTo(fst newPlayerPos, snd newPlayerPos)
                    player.RenderOn(canv)
            canv.Show()
            this.GetItem(fst newPlayerPos, snd newPlayerPos).InteractWith(player)
            if ((this.GetItem(fst newPlayerPos, snd newPlayerPos)).GetType().Equals Exit) then 
                gameOver <- true
                System.Console.Clear()
                printfn "You Escaped! Well Done!"
            elif player.IsDead() then 
                gameOver <-true
                System.Console.Clear()
                printf "You Died! Game Over!"


            //Mangler ordenlige interactioner

        
