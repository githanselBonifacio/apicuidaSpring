package com.sura.reactive.repository.jpa.testdata;

import lombok.Data;
import org.hibernate.annotations.Entity;
import org.springframework.data.annotation.Id;

import java.util.Date;

@Data
@Entity
class DataEntity {
    @Id
    private String id;
    private String name;
    private Date birthDate;
    private Long size;
}
