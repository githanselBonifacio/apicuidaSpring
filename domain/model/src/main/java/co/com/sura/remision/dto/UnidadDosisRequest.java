package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class UnidadDosisRequest {
    private String idDosis;
    private String tipo;
    private String descripcion;
}
