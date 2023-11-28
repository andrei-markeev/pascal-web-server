const http = require('http');
const { MongoClient } = require('mongodb');
const { MONGO_URL } = require('./mongoUrl');

// web server in Node.js for comparison
async function start() {
    const client = await MongoClient.connect(MONGO_URL);
    const db = client.db('deskme');

    http.createServer(async function (request, response) {

        if (request.url === "/hello") {
            response.writeHead(200, { 'Content-Type': 'text/html' });
            response.end("<!DOCTYPE html><html><h1>Hello worlde!</h1></html>");
        } else if (request.url === "/json") {
            const options = {
                availableFromHour: 8,
                availableUntilHour: 17,
                altSchedule: ['Sat','Sun'],
                altAvailableFromHour: 10,
                altAvailableUntilHour: 15
            };
            response.writeHead(200, { 'Content-Type': 'application/json' });
            response.end(JSON.stringify({
                _id: 'id12356',
                tenantId: 'tenant12346',
                name: 'New \\" location""',
                address: 'Very Long Address String that contains a lot of information and even instructions, which might be relevant but might not be relevant - who knows, but they\'re still there, because the company is trying to use address field for providing additional information in the notificaiton emails.',
                latitude: 1234,
                longitude: 4321,
                email: 'newLocation@myCompany.com',
                sendICalNotifications: false,
                notificationEmail: '"notify\r\n@myCompany.com"',
                options
            }));
        } else if (request.url === "/mongo") {
            const result = await db.collection('locations').findOne({ name: 'Demo Office' });
            response.writeHead(200, { 'Content-Type': 'application/json' });
            response.end(JSON.stringify(result));
        } else if (request.url === "/mongohtml") {
            const location = await db.collection('locations').findOne({ name: 'Demo Office' });
            response.writeHead(200, { 'Content-Type': 'text/html' });
            response.end(`<!DOCTYPE html><html><h1>${ location.name }</h1><p>This location's address is: ${location.address}</p></html>`);
        } else {
            response.writeHead(404);
            response.end();
        }

    }).listen(3000, () => console.log("Listening on port 3000"));

}


start();