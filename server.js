const express = require("express")
const { reduce, fill } = require("lodash/fp")
const uuidv4 = require('uuid/v4')

const app = express()
const cors = require("cors")
app.use(cors())

const port = 8001

const randomCompanyNames = [
  "Nec Inc.",
  "Tortor At Associates",
  "Duis Sit LLP",
  "Est Associates",
  "Arcu PC",
  "Dictum Eu Inc.",
  "Suspendisse Company",
  "Dolor LLC",
  "Consequat Lectus Sit Associates",
  "Lorem Fringilla LLC",
  "Mi Lacinia Mattis Industries",
  "Tristique Aliquet Phasellus Ltd",
  "Nec Consulting",
  "Sit LLP",
  "Cras Pellentesque Corporation",
  "Nulla Associates",
  "Aliquam Erat Institute",
  "In Molestie Corporation",
  "Ut Nec Associates",
  "Lectus Ante Dictum Inc.",
  "Feugiat Tellus Inc.",
  "Duis At Lacus Inc.",
  "Hendrerit A PC",
  "Malesuada Integer Incorporated",
  "Penatibus Limited",
  "Leo Limited",
  "Amet Luctus Incorporated",
  "Nulla Cras Eu Consulting",
  "Massa Inc.",
  "Aliquet Diam Corp.",
  "Felis Purus Ac LLP",
  "Scelerisque Neque Sed Corp.",
  "Nec Company",
  "Sed Tortor Integer Corp.",
  "Eu Eros Nam PC",
  "Amet Massa Consulting",
  "Urna Vivamus Inc.",
  "Dui Corporation",
  "Eget LLC",
  "Vel Quam Dignissim Corp.",
  "Ut Sagittis Lobortis Consulting",
  "Lobortis Quis Pede Corporation",
  "Nulla Magna Company",
  "Lorem Company",
  "Ullamcorper Viverra PC",
  "Pede Praesent Incorporated",
  "Euismod Incorporated",
  "Cras Sed Leo Associates",
  "Nonummy PC",
  "Ornare Foundation",
  "Duis Ac Arcu Industries",
  "Vel Mauris Integer Inc.",
  "Senectus Et Associates",
  "Gravida Aliquam Company",
  "Pellentesque Ultricies Dignissim Corp.",
  "Vel Corp.",
  "Nascetur Ridiculus Institute",
  "Mi Lacinia Inc.",
  "Ligula Elit Pretium Incorporated",
  "Nullam Suscipit Est Industries",
  "Luctus Sit Amet Corporation",
  "Sem Elit Foundation",
  "Nullam Incorporated",
  "Imperdiet Limited",
  "Suscipit Nonummy Fusce PC",
  "Felis Eget Company",
  "Ligula Associates",
  "Diam Pellentesque Habitant Incorporated",
  "Eget Magna Foundation",
  "Bibendum Sed Inc.",
  "Justo Praesent Luctus Industries",
  "Mauris Id Sapien PC",
  "Nibh Corporation",
  "Pellentesque Eget Corp.",
  "Lacus Cras Ltd",
  "Nisi Aenean LLC",
  "Libero LLP",
  "Tempus Lorem Fringilla LLP",
  "Gravida Praesent Eu Industries",
  "Aliquam Company",
  "Elit Pede Malesuada PC",
  "Mauris Aliquam Eu Ltd",
  "Sit Amet Luctus Ltd",
  "Morbi Tristique Senectus Consulting",
  "Arcu PC",
  "Lectus LLP",
  "Tincidunt Orci Quis LLP",
  "Turpis PC",
  "Ipsum Leo PC",
  "Eget PC",
  "Vel Mauris Foundation",
  "Dui In PC",
  "Sit Amet Lorem Corporation",
  "Vitae Risus Corporation",
  "Pede Praesent Consulting",
  "Egestas Aliquam Ltd",
  "Ac Risus Industries",
  "Eu Corp.",
  "Turpis Egestas LLP",
  "Ac Nulla Corporation"
]

const accountType = [
    "Checking",
    "Savings",
    "Mutual Cow",
    "Bond... James Bond"
]

const getRandomAccountType = () =>
    accountType[Math.floor(Math.random() * 4)]

app.get("/", (req, res) => res.send("Hello World!"));
app.get("/accounts", (req, res) => {
  let id = -1
  res.json(
    reduce(
      (acc, item) => {
        id++;
        return [...acc, { id: uuidv4(), nickname: randomCompanyNames[id], type: getRandomAccountType() }]
      },
      [],
      fill(0, 100, "sup", Array(100))
    )
  );
});

app.listen(port, () => console.log(`Example app listening on port ${port}!`))
