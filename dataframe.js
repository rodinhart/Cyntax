export const empty = {
  data: {},
  keys: [],
  indices: [],
}

export const create = (data) => ({
  data,
  keys: Object.keys(data),
  indices: Array.from(
    {
      length: Math.max(
        0,
        ...Object.values(data).map((column) => column.length)
      ),
    },
    (_, index) => index
  ),
})

export const filter = (p, df) => ({
  ...df,
  indices: df.indices.filter((index) =>
    p((key) => df.data[key]?.[index] ?? null)
  ),
})
