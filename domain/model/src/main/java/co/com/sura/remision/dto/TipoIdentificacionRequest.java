package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TipoIdentificacionRequest {
    private String idTipo;
    private String nombre;
    private String codigoPos;
    private String codigoSura;
}
