/*
  ADT for representing cells in the UI and interpreter.
*/
type cell = {
  userValue : string,
  computedValue : option string,
  id : int
};

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

