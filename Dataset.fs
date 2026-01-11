module Compress.Dataset

open System
open Microsoft.Data.Sqlite

[<Literal>]
let private DefaultDbFile = "/home/user/DataGripProjects/DataSet/dataset.db"

let private getSingleColData (sql: string) =
    let connectionString = $"Data Source={DefaultDbFile};"
    use connection = new SqliteConnection(connectionString)
    connection.Open()
    use command = connection.CreateCommand()

    try
        command.CommandText <- sql
        use reader = command.ExecuteReader()

        [| while reader.Read() do
               reader.GetDouble(0) |]
    with ex ->
        raise (Exception($"错误: {ex.Message}"))

let private getMultiColData (sql: string) =
    let connectionString = $"Data Source={DefaultDbFile};"
    use connection = new SqliteConnection(connectionString)
    connection.Open()
    use command = connection.CreateCommand()

    try
        command.CommandText <- sql
        use reader = command.ExecuteReader()

        if reader.HasRows then
            let columnCount = reader.FieldCount
            // 为每一列创建一个ResizeArray
            let columnArrays = Array.init columnCount (fun _ -> ResizeArray<double>())
            // 读取所有行
            while reader.Read() do
                for i in 0 .. columnCount - 1 do
                    columnArrays[i].Add(reader.GetDouble(i))

            // 转换为具体的类型数组
            columnArrays |> Array.map _.ToArray()
        else
            [||] // 没有数据时返回空数组
    with ex ->
        raise (Exception($"错误: {ex.Message}"))


let getFoodPrices =
    let sql = "SELECT mp_price FROM wfpvam_foodprices"
    getSingleColData sql

let getCityTemp =
    let sql = "select AvgTemperature from city_temperature"
    getSingleColData sql
