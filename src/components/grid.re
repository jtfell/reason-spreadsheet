type cell = {
  userValue : string,
  computedValue : string,
  id : int
};

/* TODO: Clean up this mess. This function needs to set the computed value of the
   cell with the same ID as the one passed in, leaving all other cells the same.

   Later, this is where the interpreter can be plugged in. At this point all cells
   get an "INCOMPLETE" status and then we kick off the interpreter algorithm to set
   the computed Value.
   */
let updateCell cell cells value =>
  [|
    [|
      { userValue: "", computedValue: "", id: 1 },
      { userValue: "", computedValue: "", id: 2 },
      { userValue: "", computedValue: "", id: 3 },
    |],
    [|
      { userValue: "", computedValue: value, id: 4 },
      { userValue: "", computedValue: "", id: 5 },
      { userValue: "", computedValue: "", id: 6},
    |],
    [|
      { userValue: "", computedValue: "", id: 7 },
      { userValue: "", computedValue: "", id: 8 },
      { userValue: "", computedValue: "", id: 9 },
    |]
  |];

/* Conveniences on top of ReasonReact binding */
let se = ReasonReact.stringToElement;
let ae = ReasonReact.arrayToElement;
let getEventValue event => (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;

/* Helper render functions */
let renderCell = fun onEdit cell =>
  <input
    key=(string_of_int cell.id)
    className="cell"
    onChange=(onEdit cell)
    value=cell.computedValue
  />;

let renderRow = fun onEdit cells => <div className="row">
      (ae (Js.Array.map (renderCell onEdit) cells))
  </div>;

/* Actions for the reducer */
type action =
  | EditCell cell string;

/* Define the component and the shape of its state */
type state = {
  cells : array (array cell)
};
let component = ReasonReact.reducerComponent "Grid";
let make _children => {
  ...component,
  initialState: fun () => {
    cells: [|
      [|
        { userValue: "", computedValue: "", id: 1 },
        { userValue: "", computedValue: "", id: 2 },
        { userValue: "", computedValue: "", id: 3 },
      |],
      [|
        { userValue: "", computedValue: "", id: 4 },
        { userValue: "", computedValue: "", id: 5 },
        { userValue: "", computedValue: "", id: 6},
      |],
      [|
        { userValue: "", computedValue: "", id: 7 },
        { userValue: "", computedValue: "", id: 8 },
        { userValue: "", computedValue: "", id: 9 },
      |]
    |]
  },
  reducer: fun action state => switch action {
    | EditCell c s => {
      ReasonReact.Update { cells: (updateCell c state.cells s) }
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

