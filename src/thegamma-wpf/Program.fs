open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders
open TheGamma.TypeProviders.Pivot

// ------------------------------------------------------------------------------------------------
// Fake series and fake pivot type
// ------------------------------------------------------------------------------------------------

type Series(x, kind:string) = 
  member z.Kind = kind
  interface FSharpProvider.GenericType with 
    member z.Members = [| |]
    member z.TypeEquals t = false
    member z.TypeArguments = []
    member z.TypeDefinition = x 

let series =
  { new FSharpProvider.GenericTypeDefinition with
    member x.Members = [| |]
    member x.TypeEquals t = false
    member x.FullName = "series"
    member x.TypeParameterCount = 2
    member x.Apply tys = 
      { new FSharpProvider.GenericTypeSchema with 
          member y.Members = [| |]
          member y.TypeEquals t = false
          member y.TypeDefinition = x
          member y.TypeArguments = tys
          member y.Substitute _ =
            let isTable = 
              match tys with 
              | [ FSharpProvider.TypeSchema.Primitive(Type.Primitive(PrimitiveType.Number | PrimitiveType.Date)); 
                  FSharpProvider.TypeSchema.Primitive(Type.Primitive(PrimitiveType.Number)) ] -> "line"
              | [ _; FSharpProvider.TypeSchema.Primitive(Type.Primitive(PrimitiveType.Number)) ] -> "column"
              | [_; FSharpProvider.TypeSchema.Primitive(Type.Object(:? PivotObject))] -> "table"
              | _ -> "object"
            Series(x, isTable) :> _ } }

let types = System.Collections.Generic.Dictionary<_, _>()
let lookupNamed n = types.[n]
types.Add("series", Type.Object series)

let olympics = 
  makePivotGlobalValue "https://thegamma-services.azurewebsites.net/pdata/olympics" "olympics" lookupNamed false
    [ "Games", PrimitiveType.String
      "Year", PrimitiveType.Number
      "Sport", PrimitiveType.String
      "Discipline", PrimitiveType.String
      "Athlete", PrimitiveType.String
      "Team", PrimitiveType.String
      "Gender", PrimitiveType.String
      "Event", PrimitiveType.String
      "Medal", PrimitiveType.String
      "Gold", PrimitiveType.Number
      "Silver", PrimitiveType.Number
      "Bronze", PrimitiveType.Number ]

let typ = 
  match olympics with
  | TypeProviders.ProvidedType.GlobalValue(_, _, _, typ) -> typ 
  | _ -> failwith "makePivotGlobalValue did not return type"
types.Add("olympics", typ)

let vars = [ "olympics", typ ]

// ------------------------------------------------------------------------------------------------
// Testing the type checker & Pivot type provider
// ------------------------------------------------------------------------------------------------

let template = """
<!DOCTYPE html>
<html lang="en-us">
<body>

<script type="text/thegamma" id="s57-code">[code]</script>
<div class="thegamma" id="s57"></div>

<script type="text/javascript">
if (!thegamma) { var thegamma=[],tg="https://thegamma.net/lib/thegamma-0.1/";
["//www.google.com/jsapi",tg+"babel.min.js",tg+"core-js.min.js",tg+"require.js",tg+"embed.js"].
forEach(function(u) { document.write('<sc'+'ript type="text/javascript" src="'+u+'"></sc'+'ript>'); });
} thegamma.push("s57");
</script>

</body>
</html>"""

open System

type Resuult<'T, 'E> =
  | Success of 'T
  | Error of 'E

Log.setEnabled Set.empty 
let ctx = Binder.createContext [] "script"

let getType code = async {
  let code = "let it = " + code
  let prog, _ = code |> Parser.parseProgram
  let prog, entities = Binder.bindProgram ctx prog
  let globals = [ for n, t in vars -> Interpreter.globalEntity n [] t None ] 
  do! TypeChecker.typeCheckProgram globals entities (Interpreter.evaluate globals) prog
  let _, ent = entities.Entities |> Seq.find (function (_, { Kind = EntityKind.Variable({ Name = "it" }, _) }) -> true | _ -> false)
  let errors = TypeChecker.collectTypeErrors prog
  let errors = [ for e in errors -> code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1), e.Message ]
  return errors, ent.Type.Value }

let getTypeName code = async {
  let! _, typ = getType code 
  match typ with
  | Type.Object(:? Series as s) -> return s.Kind
  | _ -> return "object" }

let getCompletions code = async {
  let! errors, typ = getType code
  match errors, typ with
  | [], Type.Object(obj) -> return [| for m in obj.Members do if m.Name <> "preview" then yield Ast.escapeIdent m.Name |]
  | [], _ -> return failwith "Not an object"
  | _::_, _ -> return failwith "There were errors" }

open System

let tryRun code =
    let kind = getTypeName code |> Async.RunSynchronously 
    if kind <> "object" then
        let code = 
            match kind with
            | "table" -> sprintf "let data = %s\ntable.create(data)" code
            | "column" -> sprintf "let data = %s\ncompost.charts.column(data)" code
            | "line" -> sprintf "let data = %s\ncompost.charts.line(data)" code
            | _ -> failwith "Unexpected kind"
        let file = IO.Path.GetTempFileName()
        IO.File.WriteAllText(file + ".html", template.Replace("[code]", code))
        Diagnostics.Process.Start(file + ".html") |> ignore

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open NAudio

type EditorWindow() as window =
    inherit Window(Title="The Gamma")

    let editPanel = StackPanel(Orientation=Orientation.Horizontal)
    let codeBlock = TextBlock(FontSize=24.0, Margin=Thickness(100.0,100.0,0.0,0.0))
    let memberBox = ListBox(FontSize=24.0, Margin=Thickness(4.0,100.0,0.0,0.0)) //, IsDropDownOpen=true)
    do  editPanel.Children.Add(codeBlock) |> ignore
    do  editPanel.Children.Add(memberBox) |> ignore
    let dockPanel = DockPanel()
    let tray = ToolBarTray()
    do  tray.SetValue(DockPanel.DockProperty, Dock.Top)
    let toolbar = ToolBar()
    let upButton = Button(Content="Up")
    do  toolbar.Items.Add(upButton) |> ignore
    let downButton = Button(Content="Down")
    do  toolbar.Items.Add(downButton) |> ignore
    let forwardButton = Button(Content="Forward")
    do  toolbar.Items.Add(forwardButton) |> ignore
    let backButton = Button(Content="Back")
    do  toolbar.Items.Add(backButton) |> ignore
    do  tray.ToolBars.Add(toolbar)
    do  dockPanel.Children.Add(tray) |> ignore
    do  dockPanel.Children.Add(editPanel) |> ignore
    do  window.Content <- dockPanel

    let chain = ResizeArray ["olympics"]

    let update () =
        let code = chain |> String.concat "."
        codeBlock.Text <- code + "."
        let members = (getCompletions code |> Async.RunSynchronously)
        memberBox.ItemsSource <- members
        if members.Length > 0 then memberBox.SelectedItem <- members.[0]
        tryRun code

    do update ()

    let up () = if memberBox.SelectedIndex > 0 then memberBox.SelectedIndex <- memberBox.SelectedIndex-1
    let down () = memberBox.SelectedIndex <- memberBox.SelectedIndex+1

    let forward () =
        match string memberBox.SelectedItem with
        | op & ("take" | "skip") -> chain.Add(op + "(10)")
        | m -> chain.Add(m)
        update()

    let back ()=
      if chain.Count > 1 then chain.RemoveAt(chain.Count-1); update()

    do  upButton.Click.Add(fun _ -> up())
    do  downButton.Click.Add(fun _ -> down())
    do  forwardButton.Click.Add(fun _ -> forward())
    do  backButton.Click.Add(fun _ -> back())

    let timeInterval = 50
    let sampleRate = 44100
    let byteCount = int <| Math.Round(float sampleRate* float timeInterval / 1000.0)
    let audioBuffer = Array.create byteCount 0.0f
    let pitchTracker = Pitch.PitchTracker()
    let mutable lastPitch = ""

    do  pitchTracker.SampleRate <- float sampleRate
        let handler = 
            Pitch.PitchTracker.PitchDetectedHandler(fun tracker record -> 
                let name = Pitch.PitchDsp.GetNoteName(record.MidiNote, true, true);
                System.Diagnostics.Debug.WriteLine("Pitch detected "+record.MidiNote.ToString() + " " + name)
                if name <> lastPitch then
                    //lastPitch <- name
                    match name with
                    | "E 2" -> lastPitch <- name; up()
                    | "A 2" -> lastPitch <- name; down()
                    | "D 3" -> lastPitch <- name; forward()
                    | "G 3" -> lastPitch <- name; back()
                    | "E 4" -> lastPitch <- name
                    | _ -> ()
            )
        pitchTracker.add_PitchDetected(handler)


    let mutable source = null
    do  window.Loaded.Add(fun _ ->
            source <- new NAudio.Wave.WaveIn()
            source.BufferMilliseconds <- 100
            source.WaveFormat <- Wave.WaveFormat(sampleRate,8,1)
            source.DataAvailable.Add(fun e ->
                for i = 0 to byteCount-1 do
                    let byte = e.Buffer.[i]
                    let x = (float32 byte - 128.0f) / 128.0f
                    audioBuffer.[i] <- x
                pitchTracker.ProcessBuffer(audioBuffer) 
            )
            source.StartRecording()
        )

[<STAThread>]
do
    let window = EditorWindow()
    window.WindowState <- WindowState.Maximized
    Application().Run(window) |> ignore