module Game.Minecraft.Identifiers (
  itemIdToName,
  nameToItemId )
where

type ItemList = [(Int, String)]

itemIdToName :: Int -> String
itemIdToName itemId = searchDataById itemId minecraftData

nameToItemId :: String -> Int
nameToItemId itemName = searchDataByName itemName minecraftData

searchDataById :: Int -> ItemList -> String
searchDataById _ [] = ""
searchDataById itemId ((listItemId,listItemName):xs)
    | itemId == listItemId = listItemName
    | otherwise = searchDataById itemId xs

searchDataByName :: String -> ItemList -> Int
searchDataByName _ [] = -1
searchDataByName itemName ((listItemId,listItemName):xs)
    | itemName == listItemName = listItemId
    | otherwise = searchDataByName itemName xs

minecraftData :: ItemList
minecraftData = [
        (0,"Air"),
        (1,"Stone"),
        (2,"Grass Block"),
        (3,"Dirt"),
        (4,"Cobblestone"),
        (5,"Wood Planks"),
        (6,"Saplings"),
        (7,"Bedrock"),
        (8,"Water"),
        (9,"Stationary water"),
        (10,"Lava"),
        (11,"Stationary lava"),
        (12,"Sand"),
        (13,"Gravel"),
        (14,"Gold Ore"),
        (15,"Iron Ore"),
        (16,"Coal Ore"),
        (17,"Wood"),
        (18,"Leaves"),
        (19,"Sponge"),
        (20,"Glass"),
        (21,"Lapis Lazuli Ore"),
        (22,"Lapis Lazuli Block"),
        (23,"Dispenser"),
        (24,"Sandstone"),
        (25,"Note Block"),
        (26,"Bed"),
        (27,"Powered Rail"),
        (28,"Detector Rail"),
        (29,"Sticky Piston"),
        (30,"Cobweb"),
        (31,"Tall Grass"),
        (32,"Dead Bush"),
        (33,"Piston"),
        (34,"Piston Extension"),
        (35,"Wool"),
        (36,"Block moved by Piston"),
        (37,"Dandelion"),
        (38,"Rose"),
        (39,"Brown Mushroom"),
        (40,"Red Mushroom"),
        (41,"Block of Gold"),
        (42,"Block of Iron"),
        (43,"Double Slabs"),
        (44,"Slabs"),
        (45,"Bricks"),
        (46,"TNT"),
        (47,"Bookshelf"),
        (48,"Moss Stone"),
        (49,"Obsidian"),
        (50,"Torch"),
        (51,"Fire"),
        (52,"Monster Spawner"),
        (53,"Oak Wood Stairs"),
        (54,"Chest"),
        (55,"Redstone Wire"),
        (56,"Diamond Ore"),
        (57,"Block of Diamond"),
        (58,"Crafting Table"),
        (59,"Wheat"),
        (60,"Farmland"),
        (61,"Furnace"),
        (62,"Burning Furnace"),
        (63,"Sign Post"),
        (64,"Wooden Door"),
        (65,"Ladders"),
        (66,"Rails"),
        (67,"Cobblestone Stairs"),
        (68,"Wall Sign"),
        (69,"Lever"),
        (70,"Stone Pressure Plate"),
        (71,"Iron Door"),
        (72,"Wooden Pressure Plate"),
        (73,"Redstone Ore"),
        (74,"Glowing Redstone Ore"),
        (75,"Redstone Torch (inactive)"),
        (76,"Redstone Torch (active)"),
        (77,"Stone Button"),
        (78,"Snow"),
        (79,"Ice"),
        (80,"Snow Block"),
        (81,"Cactus"),
        (82,"Clay Block"),
        (83,"Sugar Cane"),
        (84,"Jukebox"),
        (85,"Fence"),
        (86,"Pumpkin"),
        (87,"Netherrack"),
        (88,"Soul Sand"),
        (89,"Glowstone"),
        (90,"Nether Portal"),
        (91,"Jack 'o' Lantern"),
        (92,"Cake Block"),
        (93,"Redstone Repeater (inactive)"),
        (94,"Redstone Repeater (active)"),
        (95,"Locked Chest"),
        (96,"Trapdoor"),
        (97,"Monster Egg"),
        (98,"Stone Bricks"),
        (99,"Huge Brown Mushroom"),
        (100,"Huge Red Mushroom"),
        (101,"Iron Bars"),
        (102,"Glass Pane"),
        (103,"Melon"),
        (104,"Pumpkin Stem"),
        (105,"Melon Stem"),
        (106,"Vines"),
        (107,"Fence Gate"),
        (108,"Brick Stairs"),
        (109,"Stone Brick Stairs"),
        (110,"Mycelium"),
        (111,"Lily Pad"),
        (112,"Nether Brick"),
        (113,"Nether Brick Fence"),
        (114,"Nether Brick Stairs"),
        (115,"Nether Wart"),
        (116,"Enchantment Table"),
        (117,"Brewing Stand"),
        (118,"Cauldron"),
        (119,"End Portal"),
        (120,"End Portal block"),
        (121,"End Stone"),
        (122,"Dragon Egg"),
        (123,"Redstone Lamp (inactive)"),
        (124,"Redstone Lamp (active)"),
        (125,"Wooden Double Slab"),
        (126,"Wooden Slab"),
        (127,"Cocoa"),
        (128,"Sandstone Stairs"),
        (129,"Emerald Ore"),
        (130,"Ender Chest"),
        (131,"Tripwire Hook"),
        (132,"Tripwire"),
        (133,"Block of Emerald"),
        (134,"Spruce Wood Stairs"),
        (135,"Birch Wood Stairs"),
        (136,"Jungle Wood Stairs"),
        (137,"Command Block"),
        (138,"Beacon"),
        (139,"Cobblestone Wall"),
        (140,"Flower Pot"),
        (141,"Carrots"),
        (142,"Potatoes"),
        (143,"Wooden Button"),
        (144,"Mob Heads"),
        (145,"Anvil"),
        (146,"Trapped Chest"),
        (147,"Weighted Pressure Plate (Light)"),
        (148,"Weighted Pressure Plate (Heavy)"),
        (149,"Redstone Comparator (inactive)"),
        (150,"Redstone Comparator (active)"),
        (151,"Daylight Sensor"),
        (152,"Block of Redstone"),
        (153,"Nether Quartz Ore"),
        (154,"Hopper"),
        (155,"Block of Quartz"),
        (156,"Quartz Stairs"),
        (157,"Activator Rail"),
        (158,"Dropper"), 
        (256,"Iron Shovel"),
        (257,"Iron Pickaxe"),
        (258,"Iron Axe"),
        (259,"Flint and Steel"),
        (260,"Apple"),
        (261,"Bow"),
        (262,"Arrow"),
        (263,"Coal"),
        (264,"Diamond"),
        (265,"Iron Ingot"),
        (266,"Gold Ingot"),
        (267,"Iron Sword"),
        (268,"Wooden Sword"),
        (269,"Wooden Shovel"),
        (270,"Wooden Pickaxe"),
        (271,"Wooden Axe"),
        (272,"Stone Sword"),
        (273,"Stone Shovel"),
        (274,"Stone Pickaxe"),
        (275,"Stone Axe"),
        (276,"Diamond Sword"),
        (277,"Diamond Shovel"),
        (278,"Diamond Pickaxe"),
        (279,"Diamond Axe"),
        (280,"Diamond Stick"),
        (281,"Bowl"),
        (282,"Mushroom Stew"),
        (283,"Golden Sword"),
        (284,"Golden Shovel"),
        (285,"Golden Pickaxe"),
        (286,"Golden Axe"),
        (287,"String"),
        (288,"Feather"),
        (289,"Gunpowder"),
        (290,"Wooden Hoe"),
        (291,"Stone Hoe"),
        (292,"Iron Hoe"),
        (293,"Diamond Hoe"),
        (294,"Gold Hoe"),
        (295,"Seeds"),
        (296,"Wheat"),
        (297,"Bread"),
        (298,"Leather Cap"),
        (299,"Leather Tunic"),
        (300,"Leather Pants"),
        (301,"Leather Boots"),
        (302,"Chain Helmet"),
        (303,"Chain Chestplate"),
        (304,"Chain Leggings"),
        (305,"Chain Boots"),
        (306,"Iron Helmet"),
        (307,"Iron Chestplate"),
        (308,"Iron Leggings"),
        (309,"Iron Boots"),
        (310,"Diamond Helmet"),
        (311,"Diamond Chestplate"),
        (312,"Diamond Leggings"),
        (313,"Diamond Boots"),
        (314,"Golden Helmet"),
        (315,"Golden Chestplate"),
        (316,"Golden Leggings"),
        (317,"Golden Boots"),
        (318,"Flint"),
        (319,"Raw Porkchop"),
        (320,"Cooked Porkchop"),
        (321,"Painting"),
        (322,"Golden Apple"),
        (323,"Sign"),
        (324,"Wooden Door"),
        (325,"Bucket"),
        (326,"Water Bucket"),
        (327,"Lava Bucket"),
        (328,"Minecart"),
        (329,"Saddle"),
        (330,"Iron Door"),
        (331,"Redstone"),
        (332,"Snowball"),
        (333,"Boat"),
        (334,"Leather"),
        (335,"Milk"),
        (336,"Brick"),
        (337,"Clay"),
        (338,"Sugar Canes"),
        (339,"Paper"),
        (340,"Book"),
        (341,"Slimeball"),
        (342,"Minecart with Chest"),
        (343,"Minecart with Furnace"),
        (344,"Egg"),
        (345,"Compass"),
        (346,"Fishing Rod"),
        (347,"Clock"),
        (348,"Glowstone Dust"),
        (349,"Raw Fish"),
        (350,"Cooked Fish"),
        (351,"Dye"),
        (352,"Bone"),
        (353,"Sugar"),
        (354,"Cake"),
        (355,"Bed"),
        (356,"Redstone Repeater"),
        (357,"Cookie"),
        (358,"Map"),
        (359,"Shears"),
        (360,"Melon"),
        (361,"Pumpkin Seeds"),
        (362,"Melon Seeds"),
        (363,"Raw Beef"),
        (364,"Steak"),
        (365,"Raw Chicken"),
        (366,"Cooked Chicken"),
        (367,"Rotten Flesh"),
        (368,"Ender Pearl"),
        (369,"Blaze Rod"),
        (370,"Ghast Tear"),
        (371,"Gold Nugget"),
        (372,"Nether Wart"),
        (373,"Potions"),
        (374,"Glass Bottle"),
        (375,"Spider Eye"),
        (376,"Fermented Spider Eye"),
        (377,"Blaze Powder"),
        (378,"Magma Cream"),
        (379,"Brewing Stand"),
        (380,"Cauldron"),
        (381,"Eye of Ender"),
        (382,"Glistering Melon"),
        (383,"Spawn Egg"),
        (384,"Bottle o' Enchanting"),
        (385,"Fire Charge"),
        (386,"Book and Quill"),
        (387,"Written Book"),
        (388,"Emerald"),
        (389,"Item Frame"),
        (390,"Flower Pot"),
        (391,"Carrot"),
        (392,"Potato"),
        (393,"Baked Potato"),
        (394,"Poisonous Potato"),
        (395,"Empty Map"),
        (396,"Golden Carrot"),
        (397,"Mob Heads"),
        (398,"Carrot on a Stick"),
        (399,"Nether Star"),
        (400,"Pumpkin Pie"),
        (401,"Firework Rocket"),
        (402,"Firework Star"),
        (403,"Enchanted Book"),
        (404,"Redstone Comparator"),
        (405,"Nether Brick"),
        (406,"Nether Quartz"),
        (407,"Minecart with TNT"),
        (408,"Minecart with Hopper"),
        (2256,"13 Disc"),
        (2257,"Cat Disc"),
        (2258,"blocks Disc"),
        (2259,"chirp Disc"),
        (2260,"far Disc"),
        (2261,"mall Disc"),
        (2262,"mellohi Disc"),
        (2263,"stal Disc"),
        (2264,"strad Disc"),
        (2265,"ward Disc"),
        (2266,"wait Disc")
    ]