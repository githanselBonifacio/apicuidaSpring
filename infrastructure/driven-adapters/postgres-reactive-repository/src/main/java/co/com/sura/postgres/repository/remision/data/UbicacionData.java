package co.com.sura.postgres.repository.remision.data;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "ubicacion")
public class UbicacionData {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column("id_ubicacion")
    @JsonIgnore
    private String idUbicacion;
    private Double latitud;
    private Double longitud;
    private String direccion;
    @Column("tipo_via")
    private String tipoVia;
    @Column("numero1")
    private String numero1;
    @Column("nro_interseccion")
    @JsonIgnore
    private String numeroInterseccion;
    @Column("numero2")
    private String numero2;
    private String barrio;
    @Column("sin_nomenclatura")
    private Boolean sinNomenclatura;
    private String municipio;
    @Column("id_regional")
    private String idRegional;
}
