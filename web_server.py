import json
import os
import argparse
from flask import Flask, jsonify, send_file, request
from flask_cors import CORS

parser = argparse.ArgumentParser()
parser.add_argument("--imgdir", type=str, required=True)
parser.add_argument("--dbpath", type=str, required=True)
args = parser.parse_args()

image_dir = args.imgdir
db_path = args.dbpath

app = Flask(__name__)
cors = CORS(app, resources={r"/*": {"origins": "*"}})

class ImageDatabase:
    def __init__(self, save_path):
        self.images = {}
        self.save_path = save_path
        if os.path.isfile(save_path):
            self.load_from_file()
    
    def discover_images(self, path):
        for file in os.listdir(path):
            if file.endswith(".jpg") or file.endswith(".jpeg") or file.endswith(".png"):
                if file not in self.images:
                    self.images[file] = {
                        "processed": False,
                        "approved": False,
                        "crop_left": 0,
                        "crop_top": 0,
                        "crop_size": 0,
                    }
    
    def load_from_file(self):
        with open(self.save_path, "r") as f:
            self.images = json.load(f)
    
    def save_to_file(self):
        with open(self.save_path, "w") as f:
            json.dump(self.images, f)

db = ImageDatabase(db_path)
db.discover_images(image_dir)
db.save_to_file()

@app.route("/get_images", methods=["GET"])
def get_images():
    flat_images = []
    for filename, image in db.images.items():
        flat_image = image.copy()
        flat_image["filename"] = filename
        flat_images.append(flat_image)
    return jsonify(flat_images)

@app.route("/get_image/<filename>", methods=["GET"])
def get_image(filename):
    path = os.path.join(image_dir, filename)
    if os.path.isfile(path):
        return send_file(path)
    return "File not found"

@app.route("/update_properties", methods=["POST"])
def update_properties():
    data = request.get_json()

    filename = data["filename"]
    processed = data["processed"]
    approved = data["approved"]
    crop_left = data["crop_left"]
    crop_top = data["crop_top"]
    crop_size = data["crop_size"]

    if filename in db.images:
        db.images[filename]["processed"] = processed
        db.images[filename]["approved"] = approved
        db.images[filename]["crop_left"] = crop_left
        db.images[filename]["crop_top"] = crop_top
        db.images[filename]["crop_size"] = crop_size
        db.save_to_file()
        return filename

    return "Unknown image"

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8080)
