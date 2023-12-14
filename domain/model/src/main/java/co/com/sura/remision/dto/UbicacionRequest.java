package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class UbicacionRequest {
    private double latitud;
    private double longitud;
    private String direccion;
    private String tipoVia;
    private String numero1;
    private String nroInterseccion;
    private String numero2;
    private String barrio;
    private boolean sinNomenclatura;
    private MunicipioRequest municipio;
}
