package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TipoNutricionRequest {
    private String tipo;
    private String idTipo;
    private String idNutricion;
    private String descripcion;
}
