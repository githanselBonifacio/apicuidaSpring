package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TipoTratamientoRequest {
    private long idTipoTratamiento;
    private String nombre;
}
