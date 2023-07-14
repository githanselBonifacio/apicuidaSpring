package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class MunicipioRequest {
    private String idMunicipio;
    private String idCiudad;
    private String nombre;
    private long latitud;
    private long longitud;
}
