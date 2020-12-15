type Color = System.ConsoleColor
type Point = (System.Char*Color*Color)


///<summary> Function to print show a given point in the console </summary>
/// <param> a Point (System.Char*Color*Color) </param>
/// <returns> unit </returns>
let PrintPoint ((c,fg,bg):Point) = 
    System.Console.ForegroundColor <- fg
    System.Console.BackgroundColor <- bg
    printf "%c" c

///<summary> Class to create a canvas to show grapics in Console. </summary>
/// <param> Constructor takes number of desired rows and collumns </param>
/// <returns> unit </returns>
type Canvas(rows:int,cols:int) = 
    
    ///Constructor
    let xy = Array2D.create rows cols (Point (' ', Color.White, Color.Black))
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

