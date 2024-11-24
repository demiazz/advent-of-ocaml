[@mel.module "./application.module.css"] external css: Js.t({..}) = "default";

[@mel.scope "performance"] external now: unit => float = "now";

let toSorted = items => items |> Array.of_list |> Js.Array.sortInPlace;

let last = items => items[Array.length(items) - 1];

let solve = (~year: int, ~day: int, ~part: int, input: string) =>
  try({
    let start = now();
    let answer = Solver.solve(~year, ~day, ~part, Reader.from_string(input));
    let finish = now();
    [@implicit_arity] Ok(answer, finish -. start);
  }) {
  | exn => Error(Printexc.to_string(exn))
  };

[@react.component]
let make = () => {
  let years = React.useMemo0(() => Solver.years |> toSorted);
  let (year, setYear) = React.useState(() => years |> last);

  let days = React.useMemo1(() => Solver.days(~year) |> toSorted, [|year|]);
  let (day, setDay) = React.useState(() => days |> last);

  React.useEffect1(
    () => {
      setDay(_ => days |> last);

      None;
    },
    [|days|],
  );

  let (isPartOneAvailable, isPartTwoAvailable) =
    React.useMemo1(() => Solver.parts(~year, ~day), [|year, day|]);

  let ((answerOne, answerTwo), setAnswers) =
    React.useState(() => (None, None));

  let (isBusy, setIsBusy) = React.useState(() => false);

  React.useEffect1(
    () => {
      setAnswers(_ => (None, None));
      setIsBusy(_ => false);

      None;
    },
    [|year, day|],
  );

  let handleSubmit =
    React.useCallback1(
      (isPartOne, isPartTwo, input) =>
        if (!isBusy) {
          setIsBusy(_ => true);

          setAnswers(_ => (None, None));

          if (isPartOne) {
            let partOne = solve(~year, ~day, ~part=1, input);

            setAnswers(((_, partTwo)) => (Some(partOne), partTwo));
          };

          if (isPartTwo) {
            let partTwo = solve(~year, ~day, ~part=2, input);

            setAnswers(((partOne, _)) => (partOne, Some(partTwo)));
          };

          setIsBusy(_ => false);
        },
      [|year, day|],
    );

  <div className=css##root>
    <h1 className=css##title> {React.string("Advent of OCaml")} </h1>
    <h2 className=css##subTitle>
      <span className=css##subTitleDelimiter> {React.string("[")} </span>
      {React.string(year |> string_of_int)}
      <span className=css##subTitleDelimiter> {React.string("]")} </span>
      <span className=css##subTitleDelimiter> {React.string("[")} </span>
      {React.string(day |> string_of_int)}
      <span className=css##subTitleDelimiter> {React.string("]")} </span>
    </h2>
    <Selector
      className=css##years
      isDisabled=isBusy
      onSelect={year => setYear(_ => year)}
      options=years
      value=year
    />
    <Selector
      className=css##days
      isDisabled=isBusy
      onSelect={day => setDay(_ => day)}
      options=days
      value=day
    />
    <Form
      className=css##form
      isDisabled=isBusy
      isPartOneAvailable
      isPartTwoAvailable
      onSubmit=handleSubmit
    />
    <div className=css##results>
      <Results partOne=answerOne partTwo=answerTwo />
    </div>
  </div>;
};
