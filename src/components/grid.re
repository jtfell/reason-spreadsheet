type cell = {
  userValue : string,
  computedValue : option string,
  id : int
};

/*
   Helper function for manipulating the grid of cells.

   This is where the interpreter can be plugged in. At this point all cells
   get an "INCOMPLETE" status and then we kick off the interpreter algorithm to set
   the computed Value.
   */
let updateRow cell cells value => Js.Array.map (fun cellToCheck =>
  if (cellToCheck.id == cell.id) {
    {
      userValue: value,
      computedValue: None,
      id: cell.id
    }
  } else {
    {
      userValue: cellToCheck.userValue,
      computedValue: None,
      id: cellToCheck.id
    }
  }
) cells;

let updateCell cell cells value => Js.Array.map (fun row => updateRow cell row value) cells;

let interpretGridContents cells => {
  Js.log cells;
  cells
};

let optionDefault default value => switch value {
  | Some a => a
  | None => default
};

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
    value=(optionDefault "" cell.computedValue)
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
      ReasonReact.Update { cells: (interpretGridContents (updateCell c state.cells s)) }
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

