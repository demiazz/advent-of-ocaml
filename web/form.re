[@mel.module "./form.module.css"] external css: Js.t({..}) = "default";

type onSubmit = (bool, bool, string) => unit;

[@react.component]
let make =
    (
      ~className: string,
      ~isDisabled: bool,
      ~isPartOneAvailable: bool,
      ~isPartTwoAvailable: bool,
      ~onSubmit: onSubmit,
    ) => {
  let (value, setValue) = React.useState(() => "");

  let areButtonsDisabled = isDisabled || value |> Js.String.length == 0;

  let handleChange = event => {
    event->React.Event.Form.target##value |> setValue;
  };

  let handlePartOne = _ => {
    onSubmit(true, false, value);
  };

  let handlePartTwo = _ => {
    onSubmit(false, true, value);
  };

  let handleBoth = _ => {
    onSubmit(true, true, value);
  };

  let rootClassName = [|css##root, className|] |> Js.Array.join(~sep=" ");

  <div className=rootClassName>
    <textarea
      className=css##input
      disabled=isDisabled
      onInput=handleChange
      placeholder="Paste your input and select which parts you want to solve"
      rows=15
      value
    />
    <div className=css##actions>
      {isPartOneAvailable
         ? <button
             className=css##action
             disabled=areButtonsDisabled
             onClick=handlePartOne>
             {React.string("[Part One]")}
           </button>
         : React.null}
      {isPartTwoAvailable
         ? <button
             className=css##action
             disabled=areButtonsDisabled
             onClick=handlePartTwo>
             {React.string("[Part Two]")}
           </button>
         : React.null}
      {isPartOneAvailable && isPartTwoAvailable
         ? <button
             className=css##action
             disabled=areButtonsDisabled
             onClick=handleBoth>
             {React.string("[Both Parts]")}
           </button>
         : React.null}
    </div>
  </div>;
};
