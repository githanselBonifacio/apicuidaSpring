package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UnidadDosisRequest {
    private String idDosis;
    private String tipo;
    private String descripcion;
}
