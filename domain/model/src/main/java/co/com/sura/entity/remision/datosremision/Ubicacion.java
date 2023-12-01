package co.com.sura.entity.remision.datosremision;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Ubicacion {
    private double latitud;
    private double longitud;
    private String direccion;
    private String tipoVia;
    private String numero1;

    @JsonProperty("nroInterseccion")
    private String numeroInterseccion;
    private String numero2;
    private String barrio;
    private boolean sinNomenclatura;
    private String municipio;
    private String idRegional;
}
