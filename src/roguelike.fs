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
    override this.RenderOn(canvas:Canvas) = ()

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

    abstract member InteractWith : Player -> unit
    default this.InteractWith (player:Player) = ()
    member this.FullyOccupy() : bool = occupie

type Wall (x:int, y:int, bg:Color) =
    inherit Item(x, y, true)
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White,bg)
    
type Water (x:int, y:int) =
    inherit Item(x, y, false)
    override this.InteractWith (player:Player) = player.Heal 2
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White,Color.Blue)

type Fire(x:int, y:int) =
    inherit Item(x, y, false)
    override this.InteractWith (player:Player) = player.Damage 2
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.Yellow)

type FleshEatingPlant (x:int, y:int) = 
    inherit Item(x, y, true)
    override this.InteractWith (player:Player) = player.Damage 5
    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.DarkGreen)

type Exit (x:int, y:int) = 
    inherit Item(x, y, true)
    override this.InteractWith (player:Player) = printfn "You Escaped! Well Done!"

    override this.RenderOn (canvas:Canvas) = canvas.Set(this.X,this.Y,' ',Color.White, Color.Black)
    


    




    
