import json
import os
import argparse
from PIL import Image

def crop_and_pad_image(img_path, crop_left, crop_top, crop_size, extend_mode):
	bg_color = (255, 255, 255) if extend_mode == "white" else (0, 0, 0)
	out_img = Image.new("RGB", (crop_size, crop_size), bg_color)

	img = Image.open(img_path)
	out_img.paste(img, (-crop_left, -crop_top))

	return out_img

parser = argparse.ArgumentParser()
parser.add_argument("--imgdir", type=str, required=True)
parser.add_argument("--dbpath", type=str, required=True)
parser.add_argument("--outdir", type=str, required=True)
args = parser.parse_args()

with open(args.dbpath, "r") as f:
	db = json.load(f)

for filename, metadata in db.items():
	if not metadata["approved"]:
		continue

	img_path = os.path.join(args.imgdir, filename)
	if not os.path.exists(img_path):
		print(f"Image does not exist: '{img_path}'")
		continue

	crop_left = metadata["crop_left"]
	crop_top = metadata["crop_top"]
	crop_size = metadata["crop_size"]
	img = crop_and_pad_image(img_path, crop_left, crop_top, crop_size)

	out_path = os.path.join(args.outdir, filename)
	img.save(out_path)
