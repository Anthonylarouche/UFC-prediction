import express from "express";
import jsonData from './Data/ufc_predictions.json' with { type: 'json' };


const app = express();
const port = 3000;
app.use(express.static("public"));
app.use(express.urlencoded({ extended: true }));

app.get("/", (req, res) => {
  res.render("index.ejs",{data: jsonData});

});

app.listen(port, () => {
  console.log(`Listening on port ${port}`);
});