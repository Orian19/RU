const express = require("express");
const path = require("path");
const app = express();
const PORT = process.env.PORT || 5000;

app.use(express.static(path.join(__dirname, "public")));
app.use(express.json());
let counter = 0;

app.get("/api/count", (req, res) => {
    res.json({count: counter});
});

app.post("/api/count", (req, res) => {
    counter++;
    res.json({count: counter});
});

app.listen(PORT, () => {
    console.log(`Server started on port ${PORT}!`)
});
