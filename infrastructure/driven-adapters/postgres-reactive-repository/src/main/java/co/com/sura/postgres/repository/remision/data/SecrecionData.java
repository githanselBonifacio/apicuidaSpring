package co.com.sura.postgres.repository.remision.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "secrecion")
public class SecrecionData {

    @Id
    @Column("id_secrecion")
    private Integer idSecrecion;

    @Column("dias_tratamiento")
    private Integer diasTratamiento;

    @Column("envio_aspirador")
    private boolean envioAspirador;

    @Column("visita_enfermeria")
    private boolean visitaEnfermeria;

    @Column("tipo_sonda")
    private String tipoSonda;

    @Column("nasal")
    private boolean isNasal;

    @Column("traqueostomia")
    private boolean isTraqueostomia;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;
}
