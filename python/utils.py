import arcpy
import math
from collections import defaultdict
from datetime import datetime

def calc_angle(geometrie):
    """
    Berechnet den absoluten Winkel einer geraden Linie (0-180°).
    Unabhängig von der Digitalisierungsrichtung.
    
    Parameter:
        geometrie  - arcpy SHAPE@ Geometrie-Objekt
    
    Rückgabe:
        Winkel in Grad (0-180°)
    """
    part = geometrie.getPart(0)  # Line-Features sind nicht Mulitpart
    start = part[0]  # Erster Vertexpunkt
    ende  = part[part.count - 1]  # Letzter Vertexpunkt
    
    dx = ende.X - start.X  # Differenz auf x-Achse Richtung
    dy = ende.Y - start.Y  # Differenz auf y-Achse Richtung

    # atan2 gibt den Winkel im Bogenmass (mit Vorzeichen) zurück, deshalb %180 (-45°/45° ist dieselbe Richtung)
    # atan2 wird verwendet, da es mit dx == 0 umgehen kann, im Gegensatz zu atan
    winkel = math.degrees(math.atan2(dy, dx)) % 180  
    
    return round(winkel, 0)

def check_angle_similarity(angle_1, angle_2, tolerance):
    """ 
    Überprüft ob zwei Winkel ähnlich sind, d.h. innerhalb einer best. Toleranz liegen
     
    Parameter:
        angle_1     - Winkel 1
        angle_2     - Winkel 2
        tolerance   - Toleranz in Grad
         
    Rückgabe:
        True, wenn Winkel ähnlich
        False, wenn Winkel nicht ähnlich 
    """
    tol_gross = 180 - tolerance
    tol_klein = tolerance
    diff = abs(angle_1-angle_2)
    if tol_klein < diff < tol_gross:  # wenn Winkeldiff zwischen zB 10 und 170° -> False, also unähnliche Winkel
        return False
    else:
        return True

def segment_from_line(line, percentage_1, percentage_2):
    """ 
    Berechnet Segment einer Linie mit der Angabe von Start- und Endpunkt durch
    prozentuale Angabe  
    
    Parameter:
        line            - arcpy SHAPE@ Geometrie-Objekt
        percentage_1    - Dezimalangabe für Startpunkt
        percentage_2    - Dezimalangabe für Endpunkt
        
    Rückgabe:
        arcpy SHAPE@ Geometrie-Objekt
        """
    linepart = line.segmentAlongLine(percentage_1, percentage_2, use_percentage=True)

    return linepart

def make_list_by_key(input_list, key):
    """
    Gruppiert eine Liste von Dictionaries nach einem bestimmten Key.
    
    Parameter:
        input_list  - Liste von Dictionaries
        key         - Key nach welchem gruppiert wird
        
    Rückgabe:
        dict mit gruppierten Listen
    """
    list_by_key = defaultdict(list)

    for f in input_list:
        list_by_key[f.get(key)].append(f)

    return list_by_key

def make_layer(fc, name, where=None):
    """Erstellt einen Feature Layer, löscht ihn zuerst falls er existiert"""
    if arcpy.Exists(name):
        arcpy.management.Delete(name)
    if where:
        arcpy.management.MakeFeatureLayer(fc, name, where)
    else:
        arcpy.management.MakeFeatureLayer(fc, name)
    return name

def delete_add_field(fc,name,datatype):
    """ Erstellt ein Feld in einer FC, löscht es zuerst falls es existiert """
    fields = [f.name for f in arcpy.ListFields(fc)]

    if name in fields:
        arcpy.management.DeleteField(fc, name)
    
    arcpy.management.AddField(fc, name, datatype)
    
def field_type_to_text(input_fc, field, field_len=500):
    """
    Erstellt ein temporäres TEXT-Feld falls das Input-Feld kein TEXT ist.
    
    Parameter:
        fc         - Feature Class
        field_name - Name des Feldes
    
    Rückgabe:
        field_name        - falls bereits TEXT
        "join_key_temp"   - falls konvertiert
    
    """

    if field.type != "String":
        arcpy.management.AddField(input_fc, f"{field.name}_temp", "TEXT", field_length=field_len)
        arcpy.management.CalculateField(input_fc, f"{field.name}_temp", f"str(!{field.name}!)", "PYTHON3")
        return f"{field.name}_temp"
    
    return field.name

def calc_velocity(starttime, endtime, lenght):
    time_diff = endtime - starttime
    meter_per_second = lenght / time_diff
    
def parse_date(value, oid = ""):
    """
    Konvertiert ein Datumsfeld (Text oder datetime) in ein Python datetime-Objekt.
    Gibt None zurück wenn das Parsing fehlschlägt.

    Diverse mögliche Datumsvarianten sind hart codiert. Bei Fehlschlagen muss Datensatz überprüft werden 
    und allenfalls eine weitere Variante codiert werden!
    """
    if value is None:
        raise ValueError(f"Datumsfeld in Feature OID={oid} ist None!")

    if isinstance(value, datetime):  # wenn bereits Datumsfeld -> Wert wieder zurückgeben
        return value
    
    if isinstance(value, str):  # Wenn ein String -> in Datumsfeld umwandeln
        formats = [
            "%d.%m.%Y",             # 15.06.2021
            "%Y-%m-%d",             # 2021-06-15
            "%d/%m/%Y",             # 15/06/2021
            "%Y%m%d",               # 20210615
            "%d.%m.%Y %H:%M:%S",    # 15.06.2021 00:00:00
        ]
        for f in formats:  # Überprüft die Varianten möglicher String-Datumsformate und wandelt sie, falls in formats erfasst, in Datumsobjekte um
            try:
                return datetime.strptime(value.strip(), f)
            except ValueError:
                continue

        # Wenn kein Format passt: Programm abbrechen!
        raise ValueError(
            f"Datumsfeld in Feature OID={oid}: Wert '{value}' konnte nicht in ein Datumsformat umgewandelt werden. Variante muss in Funktion 'utils.parse_date' ergänzt werden!"
        )
    raise ValueError(
    f"Datumsfeld in Feature OID={oid}': Unbekannter Datumstyp! Variante muss in Funktion 'utils.parse_date' ergänzt werden!"
    )

def get_leaf_class(date):
    """
    Gibt die Vegetationsperiode eines Datums zurück.
    
    leaf-off:  01.01 - 31.03 und 15.11 - 31.12
    spring:    01.04 - 14.06
    leaf-on:   15.06 - 30.09
    fall:      01.10 - 14.11
    """

    md = (date.month, date.day)

    if md < (3,31):
        return "leaf-off"
    elif md <= (6, 14):
        return "spring"
    elif md <= (9,30):
        return "leaf-on"
    elif md <= (11,14):
        return "fall"
    else:
        return "leaf-off"