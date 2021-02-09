module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = {X=x;Y=y} // helper

// add your own functions as needed
// let printText w name index len = 
//     let spacing = (w * 18) / len
    
//     tspan [
//         X 43.;
//         Y (w * 7 + index * spacing);
//     ] [str <| sprintf "%i" name]

let buildTextList w (indexList: int list) (nameList: int list) =
    List.map2 (fun i j -> 
        let spacing = (w * 18) / indexList.Length
    
        (tspan [
            X 43.;
            Y (w * 7 + i * spacing);
        ] [str <| sprintf "%i" j])
    ) indexList nameList
    
    
//-----------------------------------------------------------------------------------------//


/// write this for Tick3 using your modified ComponentType
/// you may add to type definition in CommonTypes
let makeBusDecoderComponent (pos:XYPos) (w: int) (a: int) (n: int) = 
    {
        X = int pos.X
        Y = int pos.Y
        W = 0
        H = 0
        Type = BusDecoder(w, a, n)
    }

/// demo function - not needed for Tick3 answer
let makeDummyComponent (pos: XYPos): Component =
    { 
        X = int pos.X
        Y = int pos.Y
        W = 0 // dummy
        H = 0 // dummy
        Type = Not // dummy
    }

//-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () =
    (), Cmd.none

/// update function does nothing!
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//

/// Tick3 answer
let busDecoderView (comp: Component) = 
    let fX = float comp.X
    let fY = float comp.Y

    let w, a, n =
        match comp.Type with
        | BusDecoder(w, a, n) -> w, a, n
        | _ -> failwithf "what? Impossible case in busDecoderView for: %A" comp.Type

    let nameList = [a..n-1]
    let indexList = [0..n-1-a]
    let outputList = buildTextList w indexList nameList
    
    let scaleFactor = 1.0 // to demonstrate svg scaling
    let rotation=0 // to demonstrate svg rotation (in degrees)

    g   [ Style [ 
            // the transform here does rotation, scaling, and translation
            // the rotation and scaling happens with TransformOrigin as fixed point first
            TransformOrigin "0px 50px" // so that rotation is around centre of line
            Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
            ]
        
        ]  // g optional attributes in first list
        // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
        // use transform with scale and/or translate and/or rotate to transform group
        [
            rect [ // a demo svg polygon triangle
                SVGAttr.Width 50
                SVGAttr.Height (w * 25)
                SVGAttr.StrokeWidth "0.1px"
                SVGAttr.Stroke "Black"
                SVGAttr.FillOpacity 0.1
                SVGAttr.Fill "Grey"] []


            text [ // a demo text svg element
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "5px"
                    FontWeight "Bold"
                    Fill "Black" // demo font color
                ]
            ] ([] @ outputList)

            text [ // a demo text svg element
                X 26.; 
                Y 8.; 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "5px"
                    FontWeight "Bold"
                    Fill "Black" // demo font color
                ]
            ] [str <| sprintf "Bus Decode"]

            text [ // a demo text svg element
                X 7.; 
                Y (w * 12); 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "5px"
                    FontWeight "Bold"
                    Fill "Black" // demo font color
                ]
            ] [str <| sprintf "In"]
            
            
            // child of text element is text to display
        ] 

// demo function can be deleted
let busDecoderViewDummy (comp: Component) = 
    let fX = float comp.X
    let fY = float comp.Y 
    // in real code w,a,n would come from the component, but as the busDecoder case is not yet written this
    // is a workaround compatible with the dummy components
    let w,a,n = if fX < 100. then (3,0,8) else (4,3,5) // workaround
    //
    // This code demonstrates svg transformations, not needed for Tick3 but useful.
    // The elmish react syntax here uses CSS style transforms, not SVG attribute transforms. They are different.
    // In addition, svg elements transform under css differently from html.
    // See https://css-tricks.com/transforms-on-svg-elements/ for details if needed.
    //
    let scaleFactor=1.0 // to demonstrate svg scaling
    let rotation=0 // to demonstrate svg rotation (in degrees)
    g   [ Style [ 
            // the transform here does rotation, scaling, and translation
            // the rotation and scaling happens with TransformOrigin as fixed point first
            TransformOrigin "0px 50px" // so that rotation is around centre of line
            Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
            ]
        
        ]  // g optional attributes in first list
        // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
        // use transform with scale and/or translate and/or rotate to transform group
        [
            polygon [ // a demo svg polygon triangle
                SVGAttr.Points "10,10 900,900 10,900"
                SVGAttr.StrokeWidth "5px"
                SVGAttr.Stroke "Black"
                SVGAttr.FillOpacity 0.1
                SVGAttr.Fill "Blue"] []
            line [X1 0.; Y1 0.; X2 0.; Y2 (100.) ; Style [Stroke "Black"]] [
             // child elements of line do not display since children of svg are dom elements
             // and svg will only display on svg canvas, not in dom.
             // hence this is not used
            ]
            text [ // a demo text svg element
                X 0.; 
                Y 100.; 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "10px"
                    FontWeight "Bold"
                    Fill "Blue" // demo font color
                ]
            ] [str <| sprintf "X=%.0f Y=%.0f" fX fY] // child of text element is text to display
    ]



/// View function - in this case view is independent of model
let view (model : Model) (dispatch : Msg -> unit) =    
    [   // change for Tick3 answer
        makeBusDecoderComponent {X=50.; Y=20.} 3 0 8 // for Tick 3 two components
        makeBusDecoderComponent {X=150.; Y=20.} 4 3 5
        // makeBusDecoderComponent {X=150.; Y=20.} 4 2 16
    ] 
    |> List.map busDecoderView // change for Tick3 answer
    |> (fun svgEls -> 
        svg [
            Style [
                Border "3px solid green"
                Height 200.
                Width 300.   
            ]
        ]   svgEls )


type ValidateError =
   | WIsInvalid // ignoring a,n
   | AIsInvalid // for given w, ignoring n
   | NIsInvalid // for given a,w

/// Tick3 answer
let busDecoderValidate (comp:Component) : Result<Component, ValidateError*string> =
    match comp.Type with
    | BusDecoder(w, _, _) when w <= 0 -> Error (WIsInvalid, "w smaller than 0")
    | BusDecoder(w, a, _) when (a < 0 || a > (int(2. ** float w) - 1)) -> Error (AIsInvalid, "a not between 0 and 2^w - 1")
    | BusDecoder(w, a, n) when (a + n) > (int(2. ** float w)) -> Error (NIsInvalid, "a + n larger than 2^w")
    | BusDecoder(_, _, n) when n <= 0 -> Error (NIsInvalid, "n smaller than 0")
    | BusDecoder(_) -> Ok comp
    | _ -> failwithf "what? Impossible case in busDecoderView for: %A" comp
    


