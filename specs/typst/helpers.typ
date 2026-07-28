#let instruction(
  name,
  stack,
  exceptions,
  body,
) = [
  #table(
    columns: (20%, 80%),

    [Name], [#name],
    [Stack], [#stack],
    [Description], [
      #body
    ],
    [Exceptions], [#exceptions],
  )
  #line(length: 100%)
]
#show raw: set text(font: "Fira Code", size: 10pt)