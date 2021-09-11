import json
import os
from flask import Flask, jsonify, send_file

app = Flask(__name__)

class ImageRecord:
	def __init__(self):
		self.activated = False
		self.crop_left = 0
		self.crop_top = 0
		self.crop_size = 0

class ImageDatabase:
	def __init__(self, save_path):
		self.images = {}
		self.save_path
		if os.path.isfile(save_path):
			self.load_from_file()
	
	def discover_images(self, path):
		for file in os.listdir(path):
			if file.endswith(".jpg") or file.endswith(".jpeg") or file.endswith(".png"):
				if file not in self.images:
					self.images[file] = ImageRecord()
	
	def load_from_file(self):
		with open(self.save_path, "r") as f:
			self.images = json.load(f)
	
	def save_to_file(self):
		with open(self.save_path, "w") as f:
			json.dump(self.images, f)

image_dir = "images"
db = ImageDatabase()
db.discover_images(image_dir)
db.save_to_file()

@app.route("/get_images", methods=["GET"])
def get_images():
	return jsonify(db.images)

@app.route("/get_image/<filename>", methods=["GET"])
def get_image(filename):
	path = os.path.join(image_dir, filename)
	if os.path.isfile(path):
		return send_file(path)
	return "File not found"

@app.route("/set_image_approved/<filename>/<approved>", methods=["GET"])
def set_image_approved(filename, approved):
	if filename in db.images:
		db.images[filename].activated = (approved.lower() == "true")
		db.save_to_file()
		return filename
	return ""

@app.route("/set_image_crop/<filename>/<left>/<top>/<size>", methods=["GET"])
def set_image_crop(filename, left, top, size):
	if filename in db.images:
		db.images[filename].crop_left = int(left)
		db.images[filename].crop_top = int(top)
		db.images[filename].crop_size = int(size)
		db.save_to_file()
		return filename
	return ""

if __name__ == "__main__":
	app.run(host="0.0.0.0", port=8084)
