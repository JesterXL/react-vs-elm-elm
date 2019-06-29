const express = require('express')
const { reduce, fill } = require('lodash/fp')
const app = express()
const cors = require('cors')
app.use(cors())

const port = 8001

app.get('/', (req, res) => res.send('Hello World!'))
app.get('/accounts/dda', (req, res) => {
    let id = -1
    res.json(
        reduce(
            (acc, item) => {
                id++
                return [...acc, {id, nickname: `Nickname ${id}`, type: 'dda'}]
            },
            [],
            fill(0, 100, 'sup', Array(100))
        )
    )
})

app.listen(port, () => console.log(`Example app listening on port ${port}!`))