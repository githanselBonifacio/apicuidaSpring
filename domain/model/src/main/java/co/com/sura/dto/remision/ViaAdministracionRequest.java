package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ViaAdministracionRequest {
    private String idViaAdministracion;
    private String tipo;
    private String descripcion;
}
