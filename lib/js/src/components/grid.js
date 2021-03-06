// Generated by BUCKLESCRIPT VERSION 1.9.3, PLEASE EDIT WITH CARE
'use strict';

var Block                         = require("bs-platform/lib/js/block.js");
var Curry                         = require("bs-platform/lib/js/curry.js");
var React                         = require("react");
var Pervasives                    = require("bs-platform/lib/js/pervasives.js");
var ReasonReact                   = require("reason-react/lib/js/src/reasonReact.js");
var Cell$ReasonSpreadsheet        = require("../cell.js");
var Interpreter$ReasonSpreadsheet = require("../interpreter.js");

function se(prim) {
  return prim;
}

function ae(prim) {
  return prim;
}

function getEventValue($$event) {
  return $$event.target.value;
}

function optionDefault($$default, value) {
  if (value) {
    return value[0];
  } else {
    return $$default;
  }
}

function renderCell(onEdit, cell) {
  return React.createElement("input", {
              key: Pervasives.string_of_int(cell[/* id */2]),
              className: "cell",
              value: optionDefault("", cell[/* computedValue */1]),
              onChange: Curry._1(onEdit, cell)
            });
}

function renderRow(onEdit, cells) {
  return React.createElement("div", {
              className: "row"
            }, cells.map((function (param) {
                    return renderCell(onEdit, param);
                  })));
}

var component = ReasonReact.reducerComponent("Grid");

function make() {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function (param) {
      var reduce = param[/* reduce */3];
      return React.createElement("div", undefined, React.createElement("div", undefined, "Reason Spreadsheet"), param[/* state */4][/* cells */0].map((function (param) {
                        return renderRow((function (cell) {
                                      return Curry._1(reduce, (function (ev) {
                                                    return /* EditCell */[
                                                            cell,
                                                            ev.target.value
                                                          ];
                                                  }));
                                    }), param);
                      })));
    });
  newrecord[/* initialState */10] = (function () {
      return /* record */[/* cells : array */[
                /* array */[
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */1
                  ],
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */2
                  ],
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */3
                  ]
                ],
                /* array */[
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */4
                  ],
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */5
                  ],
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */6
                  ]
                ],
                /* array */[
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */7
                  ],
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */8
                  ],
                  /* record */[
                    /* userValue */"",
                    /* computedValue : None */0,
                    /* id */9
                  ]
                ]
              ]];
    });
  newrecord[/* reducer */12] = (function (action, state) {
      return /* Update */Block.__(0, [/* record */[/* cells */Interpreter$ReasonSpreadsheet.$$process(Cell$ReasonSpreadsheet.updateCell(action[0], state[/* cells */0], action[1]))]]);
    });
  return newrecord;
}

exports.se            = se;
exports.ae            = ae;
exports.getEventValue = getEventValue;
exports.optionDefault = optionDefault;
exports.renderCell    = renderCell;
exports.renderRow     = renderRow;
exports.component     = component;
exports.make          = make;
/* component Not a pure module */
