package co.com.sura.services.mapbox;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
public class GeoUbicacion {
    private  Double latitud;
    private Double longitud;
}
