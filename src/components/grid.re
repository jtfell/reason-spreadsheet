open Cell;

/* Conveniences on top of ReasonReact binding */
let se = ReasonReact.stringToElement;
let ae = ReasonReact.arrayToElement;
let getEventValue event => (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;

/* Couldn't find where this is in the stdlib */
let optionDefault default value => switch value {
  | Some a => a
  | None => default
};

/* Helper render functions */
let renderCell = fun onEdit cell =>
  <input
    key=(string_of_int cell.id)
    className="cell"
    onChange=(onEdit cell)
    value=(optionDefault "" cell.computedValue)
  />;

let renderRow = fun onEdit cells => <div className="row">
      (ae (Js.Array.map (renderCell onEdit) cells))
  </div>;

/* Actions for the reducer */
type action =
  | EditCell Cell.cell string;

/* Define the component and the shape of its state */
type state = {
  cells : array (array Cell.cell)
};

let component = ReasonReact.reducerComponent "Grid";
let make _children => {
  ...component,
  initialState: fun () => {
    cells: [|
      [|
        { userValue: "", computedValue: None, id: 1 },
        { userValue: "", computedValue: None, id: 2 },
        { userValue: "", computedValue: None, id: 3 },
      |],
      [|
        { userValue: "", computedValue: None, id: 4 },
        { userValue: "", computedValue: None, id: 5 },
        { userValue: "", computedValue: None, id: 6},
      |],
      [|
        { userValue: "", computedValue: None, id: 7 },
        { userValue: "", computedValue: None, id: 8 },
        { userValue: "", computedValue: None, id: 9 },
      |]
    |]
  },
  reducer: fun action state => switch action {
    | EditCell c s => {
      ReasonReact.Update { cells: (Interpreter.process (updateCell c state.cells s)) }
    }
  },
  render: fun { state, reduce } => {
    <div>
      <div> (se "Spreadsheet Grid!") </div>
      (ae (Js.Array.map
          (renderRow (fun cell => reduce (fun ev => EditCell cell (getEventValue ev))))
          state.cells)
      )
    </div>
  }
};

