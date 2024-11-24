[@mel.module "./results.module.css"] external css: Js.t({..}) = "default";

type answer = result((string, float), string);

module Result = {
  [@react.component]
  let make = (~answer: answer, ~label: string) => {
    switch (answer) {
    | Ok((message, time)) =>
      <div className=css##ok>
        {React.string("Answer for ")}
        <span className=css##label> {React.string(label)} </span>
        {React.string(": ")}
        <span className=css##answer> {React.string(message)} </span>
        {React.string(" (" ++ Printf.sprintf("%.1f", time) ++ "ms)")}
      </div>
    | Error(message) =>
      <div className=css##fail>
        {React.string("Error for ")}
        <span className=css##label> {React.string(label)} </span>
        {React.string(": ")}
        <span className=css##answer> {React.string(message)} </span>
      </div>
    };
  };
};

[@react.component]
let make = (~partOne: option(answer), ~partTwo: option(answer)) => {
  let one =
    switch (partOne) {
    | Some(answer) => <Result answer label="Part 1" />
    | None => React.null
    };
  let two =
    switch (partTwo) {
    | Some(answer) => <Result answer label="Part 2" />
    | None => React.null
    };

  <> one two </>;
};
