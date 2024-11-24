[@mel.module "./selector.module.css"] external css: Js.t({..}) = "default";

module Item = {
  type onSelect = int => unit;

  [@react.component]
  let make =
      (~isChecked: bool, ~isDisabled: bool, ~onSelect: onSelect, ~value: int) => {
    let handleChange = event => {
      event->React.Event.Form.target##value |> int_of_string |> onSelect;
    };

    <label className=css##item>
      <input
        className=css##input
        checked=isChecked
        disabled=isDisabled
        onChange=handleChange
        type_="radio"
        value={value->string_of_int}
      />
      {React.string("[" ++ string_of_int(value) ++ "]")}
    </label>;
  };
};

[@react.component]
let make =
    (
      ~className: string,
      ~isDisabled: bool,
      ~onSelect: Item.onSelect,
      ~options: array(int),
      ~value: int,
    ) => {
  let rootClassName = [|css##root, className|] |> Js.Array.join(~sep=" ");

  let items =
    options
    |> Js.Array.map(~f=option =>
         <Item
           isChecked={option == value}
           isDisabled
           key={option |> string_of_int}
           onSelect
           value=option
         />
       );

  <nav className=rootClassName> {React.array(items)} </nav>;
};
