import http from 'http';
import express from 'express';
import fetch from './countries';

const app = express();
const server = http.createServer(app);
const port = process.env.PORT || 5000;

app.get('/countries.json', (_, res) => res.send(JSON.stringify(fetch())));
app.use(express.static(`${__dirname}/..`));
server.listen(port);
